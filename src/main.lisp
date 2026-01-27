;;; main.lisp
(uiop:define-package iiscv
  (:use #:cl #:LISA-LISP)
  (:shadowing-import-from #:LISA-LISP #:assert)
  (:export #:human-commit
           #:iiscv-repl
           #:*atomic-history-graph*
           #:*human-history-graph*
           #:make-atomic-commit
           #:analyze-commit-and-assert
           #:get-last-uuid-by-name
           #:get-source-form-by-uuid
           #:rebuild-image-from-human-history
           #:rebuild-image-from-atomic-history
           #:show-atomic-commit
           #:show-human-commit
           #:show-project-milestones
           #:audit-atomic-history
           #:run-all-audits))

(in-package #:iiscv)
(defun clear-all-commits()
  (setf iiscv::*atomic-history-graph* (make-instance 'cl-graph:dot-graph))
  (setf iiscv::*human-history-graph* (make-instance 'cl-graph:dot-graph))
  (setf iiscv::*function-to-uuid-map* (make-hash-table :test 'equal))
  (setf iiscv::*current-human-commit* nil)
  (setf iiscv::*last-atomic-commit-uuid* nil))


(defvar *function-to-uuid-map* (make-hash-table :test 'equal)
  "Maps function names to their last committed UUID.")


(defvar *atomic-history-graph*
  (make-instance 'cl-graph:dot-graph)
  "Graph to store all individual, atomic commits. The history for the machine.")

(defvar *last-atomic-commit-uuid* nil
  "Reference to the UUID of the last atomic commit.")


(defun make-atomic-commit (definition-form)
  "Creates a new atomic commit and extracts all the data for the quality audit."
  (setq *audit-violations* nil)
  (lisa:reset)
  (let* ((name-form (and (listp definition-form) (second definition-form)))
	 (name (if (symbolp name-form)
		   name-form
		   (intern (string-join
			    (mapcar #'princ-to-string
				    (alexandria:flatten name-form ))
			    "-"))))

	 (docstring (get-docstring definition-form))
	 (has-docstring-p (not (null docstring)))
	 (body-length (calculate-body-length definition-form))
	 (cyclomatic-complexity (calculate-cyclomatic-complexity definition-form))
	 (magic-numbers (find-magic-numbers definition-form))
	 (unused-parameters (find-unused-parameters definition-form))
	 (is-redefining-core-symbol-p (and (symbolp name) (is-redefining-core-symbol-p name)))
	 (uses-unsafe-execution-p (find-unsafe-execution-forms definition-form))
	 (contains-heavy-consing-loop-p (contains-heavy-consing-loop-p definition-form))
	 (uses-implementation-specific-symbols-p (find-implementation-specific-symbols definition-form))
	 (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         (style-critiques (clean-critic-report (with-output-to-string (*standard-output*)(lisp-critic:critique-definition definition-form)))))

    ;; Run the analysis. The global variable will be populated.
    (analyze-commit-and-assert
     :uuid commit-uuid
     :name name
     :has-docstring-p has-docstring-p
     :body-length body-length
     :cyclomatic-complexity cyclomatic-complexity
     :magic-numbers magic-numbers
     :unused-parameters unused-parameters
     :is-redefining-core-symbol-p is-redefining-core-symbol-p
     :uses-unsafe-execution-p uses-unsafe-execution-p
     :contains-heavy-consing-loop-p contains-heavy-consing-loop-p
     :uses-implementation-specific-symbols-p uses-implementation-specific-symbols-p
     :style-critiques style-critiques)

    ;; Package all data, including LISA's results.
    (let ((commit-data `(:uuid ,commit-uuid
			 :source-form ,definition-form
			 :symbol-name ,name
			 :message ,(or docstring "No docstring provided for this commit.")
			 :timestamp ,(get-universal-time)
			 :rules-violations ,*audit-violations*)))

      (cl-graph:add-vertex *atomic-history-graph* commit-data)
      (when *last-atomic-commit-uuid*
	(cl-graph:add-edge-between-vertexes *atomic-history-graph* *last-atomic-commit-uuid* commit-uuid))
      (setf *last-atomic-commit-uuid* commit-uuid)
      (when (symbolp name)
	(let ((fully-qualified-name (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))
	  (setf (gethash fully-qualified-name *function-to-uuid-map*) commit-uuid)))

      (format t "~%Violations detected: ~A~%" (length *audit-violations*))
      (format t "~{~a~%~}" (mapcar #'car *audit-violations*))
      commit-uuid)))


(defun string-join (list-of-strings separator)
  "Joins a list of strings with a separator."
  (format nil (format nil "~~{~~a~~^~a~~}" separator) list-of-strings))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *human-history-graph*
  (make-instance 'cl-graph:dot-graph)
  "Graph to store high-level, human-readable commits. The history for humans.")

(defvar *current-human-commit* nil
  "Reference to the UUID of the last human-level commit.")


(defun get-timestamp-of-last-human-commit ()
  "Devuelve la marca de tiempo del último commit humano, o 0 si no existen."
  (if *current-human-commit*
      (let* ((vertex (find-vertex-by-uuid *human-history-graph* *current-human-commit*))
             (data (when vertex (cl-graph:element vertex))))
        ;; Aseguramos que `data` es una lista y luego accedemos a la propiedad
        (when (and (listp data) (getf data :timestamp))
          (getf data :timestamp)))
      0))


(defun make-human-commit (message)
  "Creates a new human-level commit by automatically finding recent definitions
   and linking them to the atomic history."
  (let* ((all-symbols (cl-graph:vertexes *atomic-history-graph*))
         (last-commit-time (get-timestamp-of-last-human-commit))
         (recent-symbols (loop for vertex in all-symbols
			       for data = (cl-graph:element vertex)
			       when (and (listp data)           
					 (evenp (length data))  
					 (getf data :timestamp)
					 (> (getf data :timestamp) last-commit-time))
				 collect (getf data :symbol-name)))
	 
         (unique-symbols (remove-duplicates recent-symbols :test #'equal)))

    (let* ((atomic-uuids (loop for sym in unique-symbols
                               for uuid = (get-last-uuid-by-name sym)
                               when uuid
                                 collect uuid))
           (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
           (commit-data `(:uuid ,commit-uuid
                           :message ,message
                           :atomic-uuids ,atomic-uuids
                           :timestamp ,(get-universal-time))))

      (cl-graph:add-vertex *human-history-graph* commit-data)
      (when *current-human-commit*
        (cl-graph:add-edge-between-vertexes *human-history-graph* *current-human-commit* commit-uuid))

      (setf *current-human-commit* commit-uuid)

      (generate-tests-for-human-commit commit-data)

      (format t "~%New human commit created for symbols: ~A~%" unique-symbols)
      commit-uuid)))



(defun generate-tests-for-human-commit (human-commit-data)
  "Generate test files for all forms in a human commit using make-file-commit."
  (let* ((atomic-uuids (getf human-commit-data :atomic-uuids)))
    (loop for atomic-uuid in atomic-uuids
          do (let* ((vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
                   (atomic-data (when vertex (cl-graph:element vertex)))
                   (form (getf atomic-data :source-form)))
               (when form
                 (make-file-commit atomic-uuid form))))))


;;;;;;;;;;;;;;;;;;;;; ver ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-human-history-file (file-path human-commit-data)
  "Generates a .lisp file with the given filename inside the output_src directory
   and writes the source forms of the atomic commits into it."
  (let* ((atomic-uuids (getf human-commit-data :atomic-uuids))
         (output-dir (uiop:ensure-directory-pathname "output_src/"))
         (full-path (merge-pathnames (pathname file-path) output-dir)))

    (ensure-directories-exist output-dir)
    
    (format t "~%Writing human commit source to file: ~a~%" full-path)
    (with-open-file (stream full-path :direction :output :if-exists :supersede)
      (format stream ";;; Human Commit: ~a~%" (getf human-commit-data :message))
      (format stream ";;; UUID: ~a~%" (getf human-commit-data :uuid))
      (format stream ";;; Timestamp: ~a~%~%" (getf human-commit-data :timestamp))
      
      (dolist (uuid atomic-uuids)
        (let ((source-form (get-source-form-by-uuid uuid)))
          (when source-form
            (format stream "~%~S~%" source-form)))))
    
    (format t "File created successfully.~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun manual-human-commit (message symbols &optional (file-path nil))
  "Creates a new human-level commit by linking to atomic commits.
   The message is human-readable, and the symbols link to the atomic history."
  (let* ((atomic-uuids (loop for sym in symbols
                             for uuid = (get-last-uuid-by-name sym)
                             when uuid
                               collect uuid))
         (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         (commit-data `(:uuid ,commit-uuid
                        :message ,message
                        :atomic-uuids ,atomic-uuids
                        :timestamp ,(get-universal-time)
			:file-path,file-path)))
    
    ;; 1. Add a vertex to the human history graph
    (cl-graph:add-vertex *human-history-graph* commit-data)

    ;; 2. Add an edge from the previous human commit
    (when *current-human-commit*
      (cl-graph:add-edge-between-vertexes *human-history-graph* *current-human-commit* commit-uuid))

    ;; 3. Update the global variable
    (setf *current-human-commit* commit-uuid)
    commit-uuid))


(defun has-pending-changes-p ()
  "Devuelve T si hay commits atómicos sin commit humano asociado.
   Maneja correctamente tanto vértices completos como UUIDs sueltos."
  (handler-case
      (progn
        (unless *atomic-history-graph*
          (return-from has-pending-changes-p nil))
        
        (let* ((last-human-time (get-timestamp-of-last-human-commit))
               (all-atomic-commits (cl-graph:vertexes *atomic-history-graph*)))
          (loop for vertex in all-atomic-commits
                for data = (cl-graph:element vertex)
                ; Ignorar elementos que no son commits válidos
                when (and (listp data)
                         (getf data :uuid)      ; Verificar que tenga UUID
                         (getf data :timestamp) ; Verificar que tenga timestamp
                         (> (getf data :timestamp) last-human-time))
                  return t)))
    (error (e)
      (format t "Error en has-pending-changes-p: ~A~%" e)
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun iiscv-repl ()
  "A REPL that automatically commits top-level definition forms and handles errors gracefully."
  (in-package :iiscv)
  (let ((prompt (format nil "~A-R> " (package-name *package*))))
    (loop
      (format t "~%~A" prompt)
      (handler-case
          (let* ((form (read))
                 (form-name (and (listp form) (car form))))
            (cond ((equal form-name 'load)
                   ;; iiscv-load ya se encarga de commitear cada forma, solo llámala.
                   (apply #'iiscv-load (rest form)))
                  (t
                   ;; 1. PRIMERO: Revisa si es una forma que se debe commitear.
                   (when (get-commit-type form)
                     ;; 2. SEGUNDO: Commitea la forma fuente original ANTES de evaluarla.
                     (make-atomic-commit form))
                   ;; 3. TERCERO: Ahora evalúa la forma.
                   (let ((result (eval form)))
                     (unless (eq result :no-print)
                       (print result))))))
        (error (e)
          (format t "~%Error: ~A~%" e)
          (format t "~%Resuming...~%")))))) 




(defun iiscv-load (filename)
  "Loads a .lisp file, auditing and committing each top-level definition."
  (in-package :iiscv)
  (with-open-file (stream filename)
    (loop
      (let ((form (read stream nil :eof)))
        (if (eq form :eof)
            (return)
            (progn
              (when (get-commit-type form)
                (make-atomic-commit form))
              (eval form))))))
  (format t "~%File '~A' loaded and audited successfully.~%" filename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; GEMINI BINDS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Función auxiliar para extraer solo el código Lisp de la respuesta de Gemini
(defun extract-lisp-code (response)
  "Extrae el primer bloque de código Lisp de una respuesta de texto."
  (let* ((start (search "```lisp" response)))
    (when start
      (let* ((code-start (+ start 7))
             (end (search "```" response :start2 code-start)))
        (when end
          (subseq response code-start end))))))


;; Funciones auxiliares más robustas para extraer código
(defun extract-code-any-markdown (response)
  "Extrae código de cualquier bloque markdown (```lisp, ```common-lisp, ```)."
  (let* ((start (search "```" response)))
    (when start
      (let* ((lang-start (+ start 3))
            (newline-pos (position #\Newline response :start lang-start))
            (code-start (when newline-pos (+ newline-pos 1)))
            (end (search "```" response :start2 (or code-start lang-start))))
        (when (and code-start end)
          (subseq response code-start end))))))

(defun extract-plain-code (response)
  "Intenta evaluar la respuesta como si fuera código Lisp plano."
  (handler-case
      (progn
        (read-from-string response)
        response)
    (error () nil)))


(defun start-ai-programmer-debug (initial-task)
  "Versión corregida: Evalúa todas las formas y registra cada una en IISCV."
  (format t "~%=== Iniciando Programador IA (MODO DEBUG ROBUSTO) ===~%")
  (funcall *correction-attempt-counter* :action :increment)
  
  (let* ((full-prompt (format nil "~A~%~%TASK:~%~A" *iiscv-system-prompt* initial-task))
         (gemini-response (uiop:run-program `("gemini" "-p" ,full-prompt) 
                                            :output :string 
                                            :error-output :string)))
    
    (format t "--- RESPUESTA CRUDA DE GEMINI ---~%~A~%--------------------------------~%" gemini-response)
    
    (let* ((sanitized-code (sanitize-gemini-response gemini-response)))
      (unless sanitized-code
        (format t "Error: No se pudo extraer o sanear el código Lisp.~%")
        (return-from start-ai-programmer-debug nil))
      
      (format t "--- CÓDIGO SANEADO PARA EVALUAR ---~%~A~%-----------------------------------~%" sanitized-code)
      
      (handler-case
          (progn
            (format t "> Evaluando en imagen viva...~%")
            ;; Usamos el string directamente con el Reader Loop
            (with-input-from-string (s sanitized-code)
              (loop for form = (read s nil :eof)
                    until (eq form :eof)
                    do (let ((result (eval form)))
                         (format t "~% Evaluando forma: ~S -> ~A~%" form result)
                         ;; IMPORTANTE: Registramos cada forma en el historial
                         (make-atomic-commit form))))
            ;; Una vez evaluado todo, disparamos la auditoría
            (check-violations))
        
        ;; Corregido: 'e' es la variable que captura el error
        (error (e)
          (format t "~%¡ERROR EN EVALUACIÓN! ~A~%Tipo: ~A~%" e (type-of e)))))))


(defun check-violations ()
  "Revisa si hay violaciones y, si las hay, inicia un ciclo de corrección."
  (when *audit-violations*

    (let ((max-attempts 10)) 
	   
        
        ;; --- Condición de salida ---
        (when (>= (funcall *correction-attempt-counter* :action :get) max-attempts)
          (format t "~%=== LÍMITE DE INTENTOS ALCANZADO (~A). Deteniendo. ===~%" max-attempts)
          (funcall *correction-attempt-counter* :action :reset)
          (return-from check-violations nil)))

    
    
    (let* ((first-violation (caar *audit-violations*)) ; Obtiene la primera violación como string
           (start-pos (search "found in '" first-violation))
           (end-pos (when start-pos (search "'" first-violation :start2 (+ start-pos 10))))
           (function-name (when (and start-pos end-pos)
                            (string-upcase (subseq first-violation (+ start-pos 10) end-pos))))
           (function-symbol (when function-name (find-symbol function-name :iiscv))))
      
      (when (and function-name function-symbol)
        (let* ((fully-qualified-name (format nil "~A::~A" (package-name (symbol-package function-symbol)) (symbol-name function-symbol)))
               (source-code (get-source-form fully-qualified-name))
               (violation-messages (mapcar #'car *audit-violations*))) ; <-- OBTENER LOS MENSAJES
          
          (when source-code
            ;; --- CORRECCIÓN CLAVE EN EL PROMPT ---
            (let ((correction-prompt (format nil "The following Common Lisp function has these specific quality violations:

Violations:
~{~A~^~%~}

Please fix the function to address these violations and provide only the corrected Lisp code.

Function to fix:
```lisp
~A
```"
                                              violation-messages
                                              source-code)))
              (format t "~%> Violaciones detectadas. Iniciando corrección automática para ~A...~%" function-name)
              (format t "> Enviando a Gemini el siguiente prompt de corrección:~%~A~%" correction-prompt)
	      
              (start-ai-programmer-debug correction-prompt)
	      )))))))



(defun sanitize-gemini-response (response)
  "Limpia la respuesta de Gemini eliminando formas no deseadas como (in-package ...)."
  (let* ((code-block (or (extract-lisp-code response)
                          (extract-code-any-markdown response)
                          (extract-plain-code response))))
    (when code-block
      ;; Usa una regex para encontrar y eliminar cualquier forma (in-package ...)
      (cl-ppcre:regex-replace-all "\\(\\s*in-package\\s*[^)]+\\)\\s*" code-block ""))))




(defvar *correction-attempt-counter*
  (let ((count 0))
    (lambda (&key (action :get))
      "Closure para gestionar el contador de intentos de corrección.
       ACTION puede ser :GET, :INCREMENT o :RESET."
      (case action
        (:get count)
        (:increment (incf count))
        (:reset (setf count 0))
        (otherwise
	 (error "Acción inválida para el contador: ~A. Usa :GET, :INCREMENT o :RESET."
		action))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUX FN COMMITS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-last-uuid-by-name (name-symbol)
  "Returns the UUID of the last committed version of a function by its name."
  (let* ((package-name (package-name (symbol-package name-symbol)))
         (symbol-name (symbol-name name-symbol))
         (search-key (format nil "~A::~A" package-name symbol-name)))
    (gethash search-key *function-to-uuid-map*)))

(defun get-source-form-by-uuid (uuid)
  "Retrieves the source form of a commit by its UUID."
  (let* ((vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
         (commit-data (when vertex (cl-graph:element vertex))))
    (if commit-data
        (getf commit-data :source-form)
        (format t "Error: Commit with UUID ~A not found in atomic history.~%" uuid))))


;; Helper function to get the correct documentation type

(defvar *commit-type-registry* (make-hash-table :test 'equal)
  "A registry for associating Lisp forms with commit types.")

(defun register-commit-type (form-name commit-type)
  "Registers a new Lisp form name and its associated commit type."
  (setf (gethash form-name *commit-type-registry*) commit-type))

(defun get-commit-type  (form)
    "Returns the commit type for a given definition form from the registry."
  (let ((form-name (and (listp form) (car form))))
    (when form-name
      (gethash form-name *commit-type-registry*))))


(register-commit-type 'defun 'function)
(register-commit-type 'defmacro 'function)
(register-commit-type 'defvar 'variable)
(register-commit-type 'defparameter 'variable)
(register-commit-type 'defconstant 'variable)
(register-commit-type 'defclass 'type)
(register-commit-type 'defstruct 'type)
(register-commit-type 'ql:quickload 'dependency)


(defun show-atomic-commit()
  "show-atomic-commit"
  (cl-graph:vertexes *atomic-history-graph*))

(defun show-human-commit()
  "show-human-commit"
  (cl-graph:vertexes *human-history-graph*))



;;;;;;;;;; Granular class actions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-class-source-form (class-name)
  "Retrieves the DEFCLASS form for a given class from the atomic history."
  (let* ((uuid (gethash (format nil "~A::~A" (package-name (symbol-package class-name))
                                (symbol-name class-name))
                        *function-to-uuid-map*)))
    (when uuid
      (get-source-form-by-uuid uuid))))

(defun find-slot-in-form (slot-name form)
  "Finds and returns a slot definition from a DEFCLASS form."
  (when (and (listp form) (eq (car form) 'defclass))
    (find slot-name (nth 2 form) :key #'car)))

(defun filter-slots-from-form (slot-name form)
  "Removes a slot definition from a DEFCLASS form."
  (when (and (listp form) (eq (car form) 'defclass))
    (let ((slots (nth 2 form)))
      (remove slot-name slots :key #'car))))

(defmacro add-slot (class-name slot-definition)
  "Adds a new slot to an existing class and commits the change."
  `(let* ((current-form (get-class-source-form ,class-name)))
     (unless current-form
       (error "Class ~A not found in commit history." ,class-name))
     (let* ((new-slots (append (nth 2 current-form) (list ,slot-definition)))
            (new-form `(defclass ,(first (cdr current-form))
                           ,(second (cadr current-form))
                         ,new-slots)))
       (eval new-form)
       new-form)))

(defmacro remove-slot (class-name slot-name)
  "Removes a slot from an existing class and commits the change."
  `(let* ((current-form (get-class-source-form ,class-name)))
     (unless current-form
       (error "Class ~A not found in commit history." ,class-name))
     (let* ((new-slots (remove ,slot-name (nth 2 current-form) :key #'car))
            (new-form `(defclass ,(first (cdr current-form))
                           ,(second (cadr current-form))
                         ,new-slots)))
       (eval new-form)
       new-form)))

(register-commit-type 'add-slot 'slot-change)
(register-commit-type 'remove-slot 'slot-change)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-source-form (function-name &key full-commit-p)
  "Retrieves the source form of a function from the commit history.
   'function-name' must be a string, e.g., 'IISCV::SUM'.
   If 'full-commit-p' is true, returns the entire commit node."
  (let* ((upper-case-name (string-upcase function-name))
         (uuid (gethash upper-case-name *function-to-uuid-map*))
         (source-commit nil))
    (if uuid
        (setf source-commit (find-vertex-by-uuid *atomic-history-graph* uuid))
        (format t "Error: Function ~A not found in the history.~%" function-name))
    (if source-commit
        (if full-commit-p
            (cl-graph:element source-commit)
            (getf (cl-graph:element source-commit) :source-form))
        nil)))


(defun find-vertex-by-uuid (graph uuid)
  "Finds and returns a vertex in a graph by its UUID."
  (let ((found-vertex nil))
    (cl-graph:iterate-vertexes graph
			       (lambda (v)
				 (let ((vertex-data (cl-graph:element v)))
				   (when (and (listp vertex-data)
					      (stringp (getf vertex-data :UUID))
					      (string= (getf vertex-data :UUID) uuid))
				     (setf found-vertex v)))))
    found-vertex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-project-milestones ()
  "Displays the curated project history by navigating the human commits."
  (format t "~%--- Project Milestones (Human History) ---~%")
  (let ((human-commits (cl-graph:topological-sort *human-history-graph*)))
    (dolist (commit-node human-commits)
      (let ((data (get-data-from-vertex commit-node)))
        (when data
          (format t "~%* Milestone: ~A~%" (getf data :message))
          (format t "  UUID: ~A~%" (getf data :uuid))
          (format t "  Timestamp: ~A~%" (getf data :timestamp))
          (format t "  Atomic Changes: ~A~%" (getf data :atomic-uuids))))))
  (format t "--------------------------------------------~%"))

(defun audit-atomic-history ()
  "Displays the complete and detailed history by navigating the atomic commits."
  (format t "~%--- Atomic History Audit (Blockchain) ---~%")
  (let ((atomic-commits (cl-graph:topological-sort *atomic-history-graph*)))
    (dolist (commit-node atomic-commits)
      (let ((data (get-data-from-vertex commit-node)))
        (when data
          (format t "~%* Atomic Commit: ~A~%" (getf data :uuid))
          (format t "  Message: ~A~%" (getf data :message))
          (format t "  Form: ~A~%" (getf data :source-form))
          (format t "  Timestamp: ~A~%" (getf data :timestamp))
          (format t "  Violations detected: ~A~%" (mapcar #'car (getf data :rules-violations))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rove-test-form (commit-uuid form)
  "Wraps a Lisp form in a Rove deftest form for auditing."
  `(rove:deftest ,(intern (format nil "COMMIT-~A-TEST" commit-uuid) "KEYWORD")
     (rove:ok (eval ',form) "The form should evaluate without error.")))


(defun make-file-commit (commit-uuid form)
  "Writes a Rove-compatible test file for a commit."
  (let* ((filepath (merge-pathnames (format nil "audits/~A.lisp" commit-uuid)
                                    (asdf:system-source-directory :iiscv))))
    (ensure-directories-exist filepath)
    (with-open-file (stream filepath
			    :direction
			    :output
			    :if-exists
			    :supersede)
      (let ((*print-case* :downcase))
        (format stream "~S" (make-rove-test-form commit-uuid form))))))


(defun run-all-audits ()
  "Runs all audit tests loaded into the system by evaluating the audit files."
  (format t "~%Running all audits...~%")
  (let ((audit-dir (merge-pathnames "audits/" (asdf:system-source-directory :iiscv))))
    (unless (uiop:directory-exists-p audit-dir)
      (format t "Error: Audit directory not found at ~A~%" audit-dir)
      (return-from run-all-audits nil))
    (let ((audit-files (uiop:directory-files audit-dir "*.lisp")))
      (if audit-files
          (progn
            (dolist (file audit-files)
              (format t "Loading audit file: ~A~%" file)
              (load file))
            (format t "~%All audit files loaded. Running tests...~%")
            (rove:run-suite :iiscv))
          (format t "No audit files found in ~A~%" audit-dir))))
  (format t "All audits completed.~%"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-development-image (path)
  "Saves a development image with all history and audit data.
   This image is useful for continuing development from the current state."
  (when (has-pending-changes-p)
    (format t "~%ERROR: Cannot save development image. There are pending atomic changes.~%")
    (format t "Please run (make-human-commit \"Descriptive message\") to consolidate changes before continuing.~%")
    (return-from save-development-image nil))
  
  (format t "~%Saving full development image...~%")
  #+sbcl (sb-ext:save-lisp-and-die path :executable t)
  #+ccl (ccl:save-application path :prepend-kernel t)
  #+abcl (ext:save-application path :executable t)
  #+ecl (ext:save-executable path :toplevel #'iiscv-repl))


(defun save-development-image-with-mcp (path)
  "Guarda una imagen de desarrollo completa que arranca automáticamente el servidor MCP.
   Solo permite el guardado si no hay cambios atómicos pendientes."
  (when (has-pending-changes-p)
    (format t "~%ERROR: No se puede salvar la imagen. Hay cambios atómicos pendientes.~%")
    (format t "Por favor, consolida tus cambios con (make-human-commit \"...\") antes de continuar.~%")
    (return-from save-development-image-with-mcp nil))
  
  (let ((mcp-func (find-symbol "RUN-SERVER" :cl-mcp-server)))
    (if mcp-func
        (progn
          (format t "~%[IISCV] Persistiendo cerebro industrial con servidor MCP...~%")
          (format t "[IISCV] Ruta: ~A~%" path)
          #+sbcl (sb-ext:save-lisp-and-die path 
                                           :executable t 
                                           :toplevel (symbol-function mcp-func))
          #-(or sbcl) (error "Función de guardado solo implementada para SBCL por ahora."))
        (error "Error: No se encontró el paquete CL-MCP-SERVER o la función RUN-SERVER."))))



(defun save-production-image (path)
  "Creates a lightweight production image by rebuilding the system from human commits.
   This approach is non-destructive and more robust."
  (when (has-pending-changes-p)
    (format t "~%ERROR: Cannot create production image. There are pending atomic changes.~%")
    (format t "Please run (make-human-commit \"Descriptive message\") to consolidate changes before continuing.~%")
    (return-from save-production-image nil))
  
  (format t "~%Creating lightweight production image...~%")
  
  ;; 1. Rebuild the system from the human-curated history
  (rebuild-image-from-human-history)

  ;; 2. Once rebuilt, save the new, clean image. The history graphs are not
  ;;    included in this new image because they are not part of the `rebuild`.
  (format t "~%Reconstruction complete. Saving production image to ~a...~%" path)
  #+sbcl (sb-ext:save-lisp-and-die path :executable t)
  #+ccl (ccl:save-application path :prepend-kernel t)
  #+abcl (ext:save-application path :executable t)
  #+ecl (ext:save-executable path :toplevel #'iiscv-repl))


(defun rebuild-image-from-human-history ()
  "Rebuilds the system image by evaluating the atomic commits linked to the human history."
  (unless *human-history-graph*
    (format t "Error: The human history graph is empty. There is nothing to rebuild.~%")
    (return-from rebuild-image-from-human-history nil))
  (format t "Rebuilding image from human (curated) history...~%")
  
  (let ((human-commits (cl-graph:topological-sort *human-history-graph*)))
    (dolist (human-commit-node human-commits)
      (let* ((commit-data (cl-graph:element human-commit-node))
             (atomic-uuids (getf commit-data :atomic-uuids)))
        (format t "Processing human commit: ~A~%" (getf commit-data :message))
        (dolist (atomic-uuid atomic-uuids)
          (let* ((atomic-vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
                 (atomic-data (when atomic-vertex (cl-graph:element atomic-vertex))))
            (when atomic-data
              (let ((form (getf atomic-data :source-form)))
                (format t "  -> Evaluating atomic commit ~A: ~A~%" atomic-uuid form)
                (handler-case
                    (eval form)
                  (error (e)
                    (format t "~%Error evaluating atomic commit ~A: ~A~%" atomic-uuid e))))))))))
  (format t "Reconstruction of image completed.~%"))


(defun rebuild-image-from-atomic-history ()
  "Rebuilds the system image by evaluating every atomic commit in the history. Useful for disaster recovery."
  (unless *atomic-history-graph*
    (format t "Error: The atomic history graph is empty. There is nothing to rebuild.~%")
    (return-from rebuild-image-from-atomic-history nil))
  (format t "Rebuilding image from atomic (complete) history...~%")
  
  (let ((vertices (cl-graph:topological-sort *atomic-history-graph*)))
    (dolist (vertex vertices)
      (let* ((commit-data (cl-graph:element vertex))
             (form (getf commit-data :source-form)))
        (format t "Evaluating commit ~A: ~A~%" (getf commit-data :uuid) form)
        (handler-case
            (eval form)
          (error (e)
            (format t "~%Error evaluating commit ~A: ~A~%" (getf commit-data :uuid) e)))))
    (format t "Reconstruction of image completed.~%")))


(defun get-data-from-vertex (vertex)
  "Helper function to safely get the property list from a cl-graph vertex."
  (let ((element (cl-graph:element vertex)))
    (when (listp element)
      element)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-source-code (&optional (output-dir #p"output_src/"))
  "Dumps all source code from human commits into files. Extracts only the latest versions of each symbol to avoid duplication."

  (let ((processed-symbols (make-hash-table :test 'equal)) 
        (final-forms '())) 
    
    
    (loop for human-vertex in (cl-graph:topological-sort *human-history-graph*)
          for human-data = (cl-graph:element human-vertex)
          for atomic-uuids = (getf human-data :atomic-uuids)
          do (loop for atomic-uuid in atomic-uuids
                  do (let* ((atomic-vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
                           (atomic-data (cl-graph:element atomic-vertex))
                           (form (getf atomic-data :source-form))
                           (symbol-name (getf atomic-data :symbol-name)))
                       
    
                       (when (and symbol-name
                                  (listp form))
                         
    
                         (unless (gethash symbol-name processed-symbols)
                           (push form final-forms)
                           (setf (gethash symbol-name processed-symbols) t))))))
    
    
    (ensure-directories-exist output-dir)
    
    
    (with-open-file (stream (merge-pathnames "dump.lisp" output-dir)
                           :direction :output
                           :if-exists :supersede)
      (dolist (form (reverse final-forms))
        (format stream "~S~%" form))
      (format stream "~%~%;; Generated by IISCV dump-source-code~%")
      (format stream ";; Timestamp: ~A~%" (get-universal-time)))
    
    (format t "Dump source: ~A~%" (merge-pathnames "dump.lisp" output-dir))
    (length final-forms)))


(defun dump-source-code-by-commit-type (&optional (output-dir #p"output_src/"))
  "Dumps the source code grouped by the type of commit recorded."

  (let ((processed-symbols (make-hash-table :test 'equal))
        (forms-by-commit-type (make-hash-table :test 'equal))) ; commit-type -> lista de formas
    ; Loop through human commits and group by commit type
    (loop for human-vertex in (cl-graph:topological-sort *human-history-graph*)
          for human-data = (cl-graph:element human-vertex)
          for atomic-uuids = (getf human-data :atomic-uuids)
          do (loop for atomic-uuid in atomic-uuids
                  do (let* ((atomic-vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
                           (atomic-data (cl-graph:element atomic-vertex))
                           (form (getf atomic-data :source-form))
                           (symbol-name (getf atomic-data :symbol-name)))
                       
		       
                       (when (and symbol-name
                                  (listp form))
                         
                         
                         (let ((commit-type (get-commit-type form)))
                           (when commit-type
                             
                             (unless (gethash symbol-name processed-symbols)
                               (push form (gethash commit-type forms-by-commit-type))
                               (setf (gethash symbol-name processed-symbols) t))))))))
    
    
    (ensure-directories-exist output-dir)
    
    
    (with-open-file (main-stream (merge-pathnames "main.lisp" output-dir)
                                :direction :output
                                :if-exists :supersede)
      (format main-stream ";; Generated by IISCV dump-source-code-by-commit-type~%")
      (format main-stream ";; Timestamp: ~A~%~%" (get-universal-time))
      (loop for commit-type being the hash-keys of forms-by-commit-type
            for filename = (case commit-type
                             (function "functions.lisp")
                             (variable "variables.lisp")
                             (type "types.lisp")
                             (slot-change "slot-changes.lisp")
                             (dependency "dependencies.lisp")
                             (t (format nil "~a.lisp" commit-type)))
            do (format main-stream "(load \"~A\")~%" filename)))
    
    (loop for commit-type being the hash-keys of forms-by-commit-type
          for forms being the hash-values of forms-by-commit-type
          for filename = (case commit-type
                           (function "functions.lisp")
                           (variable "variables.lisp")
                           (type "types.lisp")
                           (slot-change "slot-changes.lisp")
                           (dependency "dependencies.lisp")
                           (t (format nil "~a.lisp" commit-type)))
          when forms
          do (with-open-file (stream (merge-pathnames filename output-dir)
                                   :direction :output
                                   :if-exists :supersede)
               (format stream ";; ~A definitions~%" (string-capitalize (string commit-type)))
               (format stream ";; Generated by IISCV~%")
               (dolist (form (reverse forms))
                 (format stream "~S~%" form))))
    
    (format t "Source code dumped by commit type in: ~A~%" output-dir)))





