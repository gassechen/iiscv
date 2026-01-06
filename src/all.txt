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
	 (commit-uuid (format nil "~a" (uuid:make-v4-uuid))))

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
     :uses-implementation-specific-symbols-p uses-implementation-specific-symbols-p)

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
                   (when (get-commit-type form)
                     (make-atomic-commit form))
                   (apply #'iiscv-load (rest form)))
                  (t
                   (let ((result (eval form)))
                     (when (get-commit-type form)
                       (make-atomic-commit form))
                     (unless (eq result :no-print)
                       (print result))))))
        (error (e)
          (format t "~%Error: ~A~%" e)
          (format t "~%Resuming "))))))

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
;;; lisa-rules.lisp

(in-package :iiscv)

(defvar *audit-violations* nil
  "A global list to collect messages from audit rules.")

(lisa-lisp:make-inference-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge Base
;;; Defines the templates and rules for static code analysis.
;;;;;;;;;;;;;;;;;;;;;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Fact Templates
;;;
;;;  "Template for facts representing the analysis of a single code commit."

(deftemplate code-commit-analysis ()
  (slot commit-uuid)          
  (slot symbol-name)          
  (slot symbol-type)          
  (slot body-length)          
  (slot has-docstring-p)      
  (slot magic-numbers)        
  (slot unused-parameters)    
  (slot cyclomatic-complexity)
  (slot uses-unsafe-execution-p) 
  (slot uses-implementation-specific-symbols-p) 
  (slot is-redefining-core-symbol-p) 
  (slot contains-heavy-consing-loop-p))


;;;   "Template for facts that represent a violation of a quality rule."

(deftemplate violation ()
  (slot rule-id) 
  (slot severity)
  (slot message)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule collect-violations ()
  "Collects all violation facts into a global list."
  (violation (message ?msg))
  =>
  (push (list ?msg)  *audit-violations*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quality Rules
;;;

;;;
;;; 1. Maintainability (Mantenibilidad)
;;;
;;;  "Fires when a function's cyclomatic complexity exceeds the threshold (e.g., 10)."
(defrule rule-1-1-high-cyclomatic-complexity ()
  (code-commit-analysis (symbol-name ?name)
                        (cyclomatic-complexity ?cc))
  (test (> ?cc 10))
  =>
  (assert (violation (rule-id "1.1")
            (severity :error)
            (message (format nil "High cyclomatic complexity (~a) detected in '~a'. Consider refactoring." ?cc ?name)))))


;;; "Fires when a function's body length exceeds the threshold (e.g., 25)."
(defrule rule-1-2-function-too-long ()
  (code-commit-analysis (symbol-name ?name)
			(body-length ?len ))
  (test (> ?len 25))
  =>
  (assert (violation (rule-id "1.2")
            (severity :warning)
            (message (format nil "Symbol '~a' exceeds the 25-line limit (length: ~a)." ?name ?len)))))



;;;  "Fires when literal numbers (other than 0 or 1) are found in the code."
(defrule rule-1-3-magic-number-usage ()
  (code-commit-analysis (symbol-name ?name)
                        (magic-numbers ?nums))
  (test (not (null ?nums)))
  =>
  (assert (violation (rule-id "1.3")
                      (severity :warning)
                      (message (format nil "Magic numbers ~a found in '~a'. Define them as constants." ?nums ?name)))))

;;;
;;; 2. Reliability (Fiabilidad)
;;;

;;;  "Fires when a commit attempts to redefine a symbol from the COMMON-LISP package."
(defrule rule-2-2-core-symbol-redefinition ()
  (code-commit-analysis (symbol-name ?name)
			(is-redefining-core-symbol-p t))
  =>
  (assert (violation (rule-id "2.2")
	    (severity :error)
	    (message (format nil  "Attempt to redefine core symbol '~a'. This is highly dangerous." ?name)))))


;;;
;;; 3. Security (Seguridad)
;;;
;;; "Fires when a function appears to execute an external command."

(defrule rule-3-1-unsafe-command-execution ()
  (code-commit-analysis (symbol-name ?name)
                        (uses-unsafe-execution-p t))
  =>
  (assert (violation (rule-id "3.1")
                      (severity :error)
                      (message (format nil  "Unsafe command execution detected in '~a'. Ensure all inputs are sanitized." ?name)))))


;;;
;;; 4. Performance Efficiency (Eficiencia de Desempeño)
;;;
;;; "Fires when memory allocation (consing) is detected inside a performance-critical loop."

(defrule rule-4-1-consing-in-loop ()
  (code-commit-analysis (symbol-name ?name)
                        (contains-heavy-consing-loop-p t))
  =>
  (assert (violation (rule-id "4.1")
                      (severity :warning)
                      (message (format nil  "Heavy consing detected in a loop within '~a'. This may impact performance." ?name)))))


;;;
;;; 5. Functional Suitability (Idoneidad Funcional)
;;;
;;;"Fires when a function or class is defined without a docstring."
(defrule rule-5-1-missing-docstring ()
  (code-commit-analysis (symbol-name ?name)
                        (has-docstring-p nil))
  =>
  (assert (violation (rule-id "5.1")
                      (severity :info)
                      (message (format nil  "Symbol '~a' is missing a docstring." ?name)))))



;;;"Fires when a function has parameters that are declared but not used."
(defrule rule-5-2-unused-parameter ()
 (code-commit-analysis (symbol-name ?name)
		       (unused-parameters ?params))
  (test (not (null ?params)))
  =>
  (assert (violation (rule-id "5.2")
            (severity :warning)
            (message (format nil "Unused parameters ~a detected in '~a'." ?params ?name)))))

;;;
;;; 6. Portability (Portabilidad)
;;;
;;;  "Fires when code uses symbols specific to a particular Common Lisp implementation."

(defrule rule-6-1-implementation-specific-symbols ()
  (code-commit-analysis (symbol-name ?name)
                        (uses-implementation-specific-symbols-p t))
  =>
  (assert (violation (rule-id "6.1")
                      (severity :warning)
                      (message (format nil  "Use of non-portable, implementation-specific symbols detected in '~a'." ?name)))))



(defun test-all-rules ()
  "Runs a comprehensive test of all quality auditor rules."
  (lisa-lisp:reset)
  
  ;; Rule 1.1: High Cyclomatic Complexity
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-1-1")
                     (symbol-name "complex-function")
                     (body-length 0)
                     (cyclomatic-complexity 15)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 1.2: Function Too Long
  (lisa-lisp:assert (code-commit-analysis
                   (commit-uuid "test-rule-1-2")
                   (symbol-name "my-long-function")
                   (body-length 42)
                   (cyclomatic-complexity 0)
                   (magic-numbers nil)
                   (is-redefining-core-symbol-p nil)
                   (uses-unsafe-execution-p nil)
                   (contains-heavy-consing-loop-p nil)
                   (has-docstring-p t)
                   (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 1.3: Magic Number Usage
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-1-3")
                     (symbol-name "bad-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers '(5 100))
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 2.2: Core Symbol Redefinition
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-2-2")
                     (symbol-name "DEFUN")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p t)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 3.1: Unsafe Command Execution
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-3-1")
                     (symbol-name "exec-shell-command")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p t)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 4.1: Consing in Loop
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-4-1")
                     (symbol-name "loop-with-cons")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p t)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 5.1: Missing Docstring
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-5-1")
                     (symbol-name "undocumented-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p nil)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 6.1: Implementation-specific Symbols
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-6-1")
                     (symbol-name "sbcl-specific-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (has-docstring-p t)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (uses-implementation-specific-symbols-p t)))

  ;; Run the inference engine
  (lisa-lisp:run)
  
  ;; Display the final facts, which should include all the violations
  (lisa-lisp:facts))
;;; lisa-rules-aux-fn.lisp

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;FOR RULES LISA AUDITOR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-body-forms (definition-form)
  "Extracts the body from a definition form, correctly handling docstrings."
  (cond ((and (listp definition-form)
              (member (car definition-form) '(defun defmacro)))
         (let ((body (cdddr definition-form)))
           (if (and (listp body) (stringp (car body)))
               (cdr body)
               body)))
        (t
         nil)))



(defun calculate-body-length (definition-form)
  "Calculates the length of a function's body.
   This is an approximation: it counts top-level forms."
  (let ((body-forms (get-body-forms definition-form)))
    (if body-forms
        (length body-forms)
        0)))



(defun count-decision-points (form)
  "Recursively traverses a form to count control structures
   that increase cyclomatic complexity."
  (let ((count 0))
    (when (listp form)
      (case (car form)
        ((if when cond case loop dolist dolist-from-end)
         (incf count)))
      (dolist (subform (cdr form))
        (incf count (count-decision-points subform))))
    count))



(defun calculate-cyclomatic-complexity (definition-form)
  "Calculates the cyclomatic complexity of a function."
  (let ((body (and (listp definition-form)
                   (cddr definition-form))))
    (when (and body (stringp (car body)))
      (setf body (cdr body)))
    
    (+ 1 (count-decision-points body))))


;; (defun find-magic-numbers (form)
;;   "Recursively traverses a form to find literal numbers
;;    other than 0 or 1 and returns a list of them."
;;   (let ((found-numbers nil))
;;     (labels ((scan (subform)
;;                (cond ((listp subform)
;;                       (dolist (item subform)
;;                         (scan item)))
;;                      ((and (numberp subform)
;;                            (not (member subform '(0 1))))
;;                       (push subform found-numbers)))))
;;       (scan form)
;;       (reverse found-numbers))))


(defun find-magic-numbers (form)
  "Finds and returns a list of magic numbers in a Lisp form."
  (let ((found-numbers nil))
    (labels ((scan (subform)
               (cond ((numberp subform)
                      (push subform found-numbers))
                     ((listp subform)
                      ;; Check for DEFEUN, DEFMACRO, etc.
                      (let ((form-type (car subform)))
                        (when (eq form-type 'defun)
                          ;; Scan the body of the function
                          (dolist (item (cddr subform))
                            (scan item)))
                        (when (eq form-type 'defconstant)
                          ;; Do not scan the number in a defconstant
                          (return-from scan nil))
                        (when (eq form-type 'defvar)
                          ;; Do not scan the value of a defvar
                          (return-from scan nil))
                        ;; Recursively scan the rest of the list
                        (dolist (item (cdr subform))
                          (scan item)))))))
      (scan form)
      (when found-numbers
        (list found-numbers)))))



(defun find-unsafe-execution-forms (form)
  "Recursively traverses a form to find symbols associated with unsafe
   external command execution."
  (let ((found-forms nil)
        (unsafe-symbols '(uiop:run-program external-program:start)))
    (labels ((scan (subform)
               (when (listp subform)
                 (let ((car-form (car subform)))
                   ;; Check if the function call is one of the unsafe symbols
                   (when (and (symbolp car-form)
                              (member car-form unsafe-symbols))
                     (push subform found-forms))
                   ;; Continue scanning sub-forms
                   (dolist (item (cdr subform))
                     (scan item))))))
      (scan form)
      (reverse found-forms))))


(defun contains-heavy-consing-loop-p (definition-form)
  "Checks if a function definition contains heavy consing inside a loop."
  (let ((consing-detected nil)
        (consing-functions '(cons list list* make-array make-hash-table)))
    (labels ((scan (form)
               (when consing-detected
                 (return-from scan))
               (when (listp form)
                 (let ((car-form (car form)))
                   (when (member car-form '(loop dolist dotimes))
                     ;; Now, scan the body of the loop for consing functions
                     (dolist (item (cdr form))
                       (when (and (listp item)
                                  (member (car item) consing-functions))
                         (setf consing-detected t)
                         (return-from scan))))
                   (dolist (item (cdr form))
                     (scan item))))))
      (scan definition-form)
      consing-detected)))


;; (defun find-implementation-specific-symbols (form)
;;   "Recursively finds symbols that are not in a standard Common Lisp or project package."
;;   (let ((found-symbols nil)
;;         (standard-packages '(:common-lisp :keyword :cl-user :lisp :editor :iiscv)))
;;     (labels ((scan (subform)
;;                (cond ((atom subform)
;;                       (when (and (symbolp subform)
;;                                  (not (keywordp subform)))
;;                         (let* ((pkg (symbol-package subform))
;;                                (pkg-name (and pkg (package-name pkg))))
;;                           (when (and pkg
;;                                      (not (member (intern (string-upcase pkg-name) :keyword)
;;                                                   standard-packages)))
;;                             (push subform found-symbols)))))
;;                      ((listp subform)
;;                       (dolist (item subform)
;;                         (scan item))))))
;;       (scan form)
;;       (not (null found-symbols)))))


(defun find-implementation-specific-symbols (form)
  "Recursively finds symbols that are not in a standard Common Lisp or project package."
  (let ((found-symbols nil)
        (standard-packages (mapcar #'package-name (list-all-packages))))
    (labels ((scan (subform)
               (cond ((atom subform)
                      (when (and (symbolp subform)
                                 (not (keywordp subform)))
                        (let* ((pkg (symbol-package subform))
                               (pkg-name (and pkg (package-name pkg))))
                          (when (and pkg
                                     (not (member pkg-name standard-packages :test #'string-equal)))
                            (push subform found-symbols)))))
                     ((listp subform)
                      (dolist (item subform)
                        (scan item))))))
      (scan form)
      (not (null found-symbols)))))


(defun find-unused-parameters (definition-form)
  "Finds parameters in a function definition that are declared but not used."
  (let* ((params (get-parameters-list definition-form))
         (body (get-body-forms definition-form))
         (used-symbols (find-used-symbols body))
         (unused-params nil))
    (setf params (remove-if (lambda (param) 
                              (member param '(&optional &rest &key &aux 
                                               &allow-other-keys &whole &environment)))
                            params))
    
    (dolist (param params)
      (when (and (symbolp param)
                 (not (gethash param used-symbols)))
        (push param unused-params)))
    (reverse unused-params)))


(defun get-parameters-list (definition-form)
  "Extracts the parameters list from a definition form."
  (when (and (listp definition-form)
             (member (car definition-form) '(defun defmacro)))
    (let ((params (third definition-form)))
      (extract-parameter-names params))))

(defun extract-parameter-names (params)
  "Extrae solo los nombres de los parámetros, ignorando palabras clave y valores iniciales."
  (let ((result '()))
    (labels ((traverse (lst)
               (cond
                 ((null lst) nil)
                 ((member (car lst) '(&optional &rest &key &aux &allow-other-keys))
                  (traverse (cdr lst)))
                 ((consp (car lst))
                  (push (caar lst) result)
                  (traverse (cdr lst)))
                 (t
                  (push (car lst) result)
                  (traverse (cdr lst))))))
      (traverse params)
      (reverse result))))


(defun find-unused-parameters (definition-form)
  "Finds parameters in a function definition that are declared but not used."
  (let* ((params (get-parameters-list definition-form))
         (body (get-body-forms definition-form))
         (used-symbols (find-used-symbols body))
         (unused-params nil))
    (dolist (param params)
      (when (and (symbolp param)
                 (not (gethash param used-symbols)))
        (push param unused-params)))
    (reverse unused-params)))


(defun find-used-symbols (form)
  "Recursively traverses a form to find and count all symbols used."
  (let ((used-symbols (make-hash-table :test 'eq)))
    (labels ((scan (subform)
               (cond ((atom subform)
                      (when (and (symbolp subform)
                                 (not (keywordp subform))
                                 (not (member subform '(&optional &rest &key &aux 
                                                            &allow-other-keys &whole &environment))))
                        (incf (gethash subform used-symbols 0))))
                     ((listp subform)
                      (dolist (item subform)
                        (scan item))))))
      (scan form))
    used-symbols))



(defun get-docstring (definition-form)
  "Extracts the docstring from a definition form, returning NIL if none exists."
  (let ((docstring-candidate (nthcdr 2 definition-form)))
    (loop for form in docstring-candidate
          when (stringp form)
            do (return form))))




(defun is-redefining-core-symbol-p (name)
  "Placeholder for a function to check for core symbol redefinition."
  (declare (ignore name))
  nil)




(defun check-implementation-specific-symbols-for (symbol-name)
  "Checks if a symbol is likely implementation-specific by its name prefix."
  (let ((prefixes '("SB-" "ECL-" "CCL-" "ALLEGRO-" "LISPWORKS-" "CLISP-" "CMUCL-" "ABCL-")))
    (loop for prefix in prefixes
          thereis (string-starts-with-p prefix (string-upcase symbol-name)))))


(defun string-starts-with-p (prefix string)
  "Helper function to check if a string starts with a given prefix."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))





(defun analyze-commit-and-assert (&key uuid name has-docstring-p body-length
				    cyclomatic-complexity magic-numbers
				    unused-parameters
				    is-redefining-core-symbol-p
				    uses-unsafe-execution-p
				    contains-heavy-consing-loop-p
				    uses-implementation-specific-symbols-p)
  "Analiza los datos de un commit y aserta un hecho para el motor de inferencia LISA."
  (setq *audit-violations* nil)
  (lisa:reset)
  (let ((fact-data
          `(code-commit-analysis
            (commit-uuid ,uuid)
            (symbol-name ,name)
            (body-length ,body-length)
            (cyclomatic-complexity ,cyclomatic-complexity)
            (magic-numbers ',magic-numbers)
            (is-redefining-core-symbol-p ,is-redefining-core-symbol-p)
            (uses-unsafe-execution-p ,uses-unsafe-execution-p)
            (contains-heavy-consing-loop-p ,contains-heavy-consing-loop-p)
            (has-docstring-p ,has-docstring-p)
            (unused-parameters ,unused-parameters)
            (uses-implementation-specific-symbols-p ,uses-implementation-specific-symbols-p))))
    (eval `(lisa:assert ,fact-data)))
  (lisa:run))

