(uiop:define-package iiscv
  (:use #:cl)
  (:export #:human-commit
           #:iiscv-repl
           #:*atomic-history-graph*
           #:*human-history-graph*
           ;; Las siguientes son las funciones que tenías, las mantendremos por ahora
           ;;#:get-history
           ;;#:get-commit
           ;;#:get-last-commit
           ;;#:make-state-snapshot
           ;;#:get-commit-form
           ;;#:get-last-uuid-by-name
           ;;#:dump-history-to-file
           ;;#:load-history-from-file
           ;;#:rebuild-image-from-history
           ;;#:run-all-audits
	   ))


(in-package #:iiscv)

(defvar *function-to-uuid-map* (make-hash-table :test 'equal)
   "Maps function names to their last committed UUID.")


(defvar *atomic-history-graph*
  (make-instance 'cl-graph:dot-graph)
  "Graph to store all individual, atomic commits. The history for the machine.")

(defvar *last-atomic-commit-uuid* nil
  "Reference to the UUID of the last atomic commit.")


(defun make-atomic-commit (definition-form)
  "Makes a new atomic commit from any definition form."
  (let* ((name (and (listp definition-form) (second definition-form)))
         (docstring-type (get-docstring-type definition-form))
         (commit-message (and docstring-type (documentation name docstring-type)))
         (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         ;; Aquí empaquetamos todos los datos en una sola lista de propiedades.
         (commit-data `(:uuid ,commit-uuid
                         :source-form ,definition-form
                         :message ,(or commit-message "No docstring provided for this commit.")
                         :timestamp ,(get-universal-time))))
    
    ;; 1. Add a vertex to the graph, con la lista de propiedades como su elemento.
    (cl-graph:add-vertex *atomic-history-graph* commit-data)

    ;; 2. Add an edge (link) from the previous commit
    (when *last-atomic-commit-uuid*
      ;; Ahora necesitamos el UUID para el enlace, que está dentro de commit-data.
      (cl-graph:add-edge-between-vertexes *atomic-history-graph* *last-atomic-commit-uuid* commit-uuid))

    ;; 3. Update the global variables
    (setf *last-atomic-commit-uuid* commit-uuid)
    
    ;; Guarda el UUID usando el nombre completo, si es una forma de definición.
    (when name
      (let ((fully-qualified-name (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))
        (setf (gethash fully-qualified-name *function-to-uuid-map*) commit-uuid)))
    (make-file-commit commit-uuid definition-form)
    
    commit-uuid))


(defvar *human-history-graph*
  (make-instance 'cl-graph:dot-graph)
  "Graph to store high-level, human-readable commits. The history for humans.")

(defvar *current-human-commit* nil
  "Reference to the UUID of the last human-level commit.")


(defun human-commit (message &rest symbols)
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
                         :timestamp ,(get-universal-time))))
    
    ;; 1. Add a vertex to the human history graph
    (cl-graph:add-vertex *human-history-graph* commit-data)

    ;; 2. Add an edge from the previous human commit
    (when *current-human-commit*
      (cl-graph:add-edge-between-vertexes *human-history-graph* *current-human-commit* commit-uuid))

    ;; 3. Update the global variable
    (setf *current-human-commit* commit-uuid)
    
    commit-uuid))



(defun iiscv-repl ()
  "A REPL that automatically commits top-level definition forms and handles errors gracefully."
  (in-package :iiscv)
  (let ((prompt (format nil "~A-R> " (package-name *package*))))
    (loop
      (format t "~%~A" prompt)
      (handler-case
          (let* ((form (read)))
            (let ((result (eval form)))
              (when (get-docstring-type form)
                (make-atomic-commit form))
              (unless (eq result :no-print)
                (print result))))
        (error (e)
          (format t "~%Error: ~A~%" e)
          (format t "~%Resuming "))))))



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
(defun get-docstring-type (form)
  "Returns the documentation type for a given definition form."
  (case (car form)
    (defun 'function)
    (defmacro 'function)
    (defvar 'variable)
    (defparameter 'variable)
    (defconstant 'variable)
    (defclass 'type)
    (defstruct 'type)
    (ql:quickload 'dependency) 
    (t nil)))



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



(defun show-atomic-commit()
  "show-atomic-commit"
  (cl-graph:vertexes *atomic-history-graph*))

(defun show-human-commit()
  "show-human-commit"
  (cl-graph:vertexes *human-history-graph*))


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
          (format t "  Timestamp: ~A~%" (getf data :timestamp)))))))



(defun make-rove-test-form (commit-uuid form)
  "Wraps a Lisp form in a Rove deftest form for auditing."
  `(rove:deftest ,(intern (format nil "COMMIT-~A-TEST" commit-uuid) "KEYWORD")
     (rove:ok (eval ',form) "The form should evaluate without error.")))


(defun make-file-commit (commit-uuid form)
  "Writes a Rove-compatible test file for a commit."
  (let* ((filepath (merge-pathnames (format nil "audits/~A.lisp" commit-uuid)
                                    (asdf:system-source-directory :iiscv))))
    (ensure-directories-exist filepath)
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Global variables to store the history and the current commit state.
;; (defvar *history* (make-hash-table :test 'equal)
;;   "Stores commits, with the hash as the key.")

;; (defvar *current-commit* nil
;;   "Reference to the hash of the last commit.")


;; (defvar *function-to-uuid-map* (make-hash-table :test 'equal)
;;   "Maps function names to their last committed UUID.")


;; ;;; Helper function to get the correct documentation type
;; (defun get-docstring-type (form)
;;   "Returns the documentation type for a given definition form."
;;   (case (car form)
;;     (defun 'function)
;;     (defmacro 'function)
;;     (defvar 'variable)
;;     (defparameter 'variable)
;;     (defconstant 'variable)
;;     (defclass 'type)
;;     (defstruct 'type)
;;     (t nil)))


;; (defun get-function-lambda (function-name)
;;   "Returns the lambda-form of a named function."
;;   (macroexpand (list 'function function-name)))


;; (defun make-commit (definition-form)
;;   "Makes a new commit from any definition form."
;;   (let* ((name (second definition-form))
;;          (docstring-type (get-docstring-type definition-form))
;;          (commit-message (and docstring-type
;;                               (documentation name docstring-type)))
;;          (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
;;          (commit-data
;;           (list :message (or commit-message "No docstring provided for this commit.")
;;                 :changes (list (list name definition-form))
;;                 :timestamp (get-universal-time)
;;                 :parent *current-commit*))
;;          ;; Construye la clave con el nombre completo.
;;          (fully-qualified-name (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))
    
;;     (setf (gethash commit-uuid *history*) commit-data)
;;     (setf *current-commit* commit-uuid)
    
;;     ;; Guarda el UUID usando el nombre completo.
;;     (setf (gethash fully-qualified-name *function-to-uuid-map*) commit-uuid)
    
;;     (make-file-commit commit-uuid commit-data)
;;     commit-uuid))


;; (defun make-file-commit (commit-hash commit-data)
;;   "Writes a Rove-compatible test file for a commit."
;;   (let* ((filepath (merge-pathnames (format nil "audits/~A.lisp" commit-hash)
;;                                     (asdf:system-source-directory :iiscv)))
;;          (form (second (first (getf commit-data :changes)))))
;;     (ensure-directories-exist filepath)
;;     (print filepath) ; <-- Aquí está la línea de diagnóstico
;;     (with-open-file (stream filepath :direction :output :if-exists :supersede)
;;       (let ((*print-case* :downcase))
;;         (format stream "~S" (make-rove-test-form commit-hash form))))))



;; (defun make-rove-test-form (commit-hash form)
;;   "Wraps a Lisp form in a Rove deftest form for auditing."
;;   `(rove:deftest ,(intern (format nil "COMMIT-~A-TEST" commit-hash) "KEYWORD")
;;     (rove:ok (eval ',form) "The form should evaluate without error.")))

;; (defun hash-commit (commit)
;;   "A placeholder for a cryptographic hash function. Returns a unique string."
;;   (declare (ignore commit))
;;   (format nil "~A" (random 100000)))

;; (defun get-history ()
;;   "Returns the entire history hash table."
;;   *history*)

;; (defun get-commit (commit-hash)
;;   "Returns a commit object given its hash."
;;   (gethash commit-hash *history*))

;; (defun get-commit-form (commit-uuid)
;;   "Retrieves the original Lisp form from a commit in the history."
;;   (let* ((commit-data (gethash commit-uuid *history*)))
;;     (unless commit-data
;;       (format t "Error: Commit with UUID ~A not found in history.~%" commit-uuid)
;;       (return-from get-commit-form nil))
    
;;     (second (first (getf commit-data :changes)))))


;; (defun get-last-commit ()
;;   "Returns the hash of the last commit."
;;   (get-commit *current-commit*))



;; (defun make-state-snapshot (symbol-list &key (message "Snapshot de estado de variables."))
;;   "Takes a snapshot of the current state of a list of symbols and creates a commit."
;;   (let ((state-forms '()))
;;     (dolist (sym symbol-list)
;;       (when (boundp sym)
;;         (let* ((current-value (symbol-value sym))
;;                (form `(setf ,sym ',current-value)))
;;           (push form state-forms))))
    
;;     ;; Ahorraremos el snapshot como una forma de Lisp
;;     (let* ((snapshot-data `(progn ,@state-forms))
;;            (commit-data
;;              (list :message message
;;                    :changes (list (list :snapshot snapshot-data))
;;                    :timestamp (get-universal-time)
;;                    :parent *current-commit*))
;;            (commit-hash (hash-commit commit-data)))
;;       (setf (gethash commit-hash *history*) commit-data)
;;       (setf *current-commit* commit-hash)
;;       (make-file-commit commit-hash commit-data)
;;       commit-hash)))

;; (defun browse-history ()
;;   "Displays all commits stored in the in-memory history."
;;   (format t "~%--- History ---~%")
;;   (maphash (lambda (hash commit)
;;              (format t "Commit: ~A~%" hash)
;;              (format t "  Parent: ~A~%" (getf commit :parent))
;;              (format t "  Timestamp: ~A~%" (getf commit :timestamp))
;;              (format t "  Message: ~A~%~%" (getf commit :message)))
;;            *history*))


;; (defun get-last-uuid-by-name (name-symbol)
;;   "Returns the UUID of the last committed version of a function by its name."
;;   (let* ((package-name (package-name (symbol-package name-symbol)))
;;          (symbol-name (symbol-name name-symbol))
;;          (search-key (format nil "~A::~A" package-name symbol-name)))
;;     (gethash search-key *function-to-uuid-map*)))



;; (defun iiscv-repl ()
;;   "A REPL that automatically commits top-level definition forms."
;;   (in-package :iiscv)
;;   (let ((prompt (format nil "~A-R> " (package-name *package*))))
;;     (loop
;;       (format t "~%~A" prompt)
;;       (let* ((form (read))
;; 	     (doc-type (get-docstring-type form)))
;; 	(let ((result (eval form))) ; <-- Evaluar la forma primero
;; 	  (unless (eq result :no-print)
;; 	    (print result)))
;; 	(when doc-type
;; 	  (print (make-commit form))))))) 


;; (defun run-all-audits ()
;;   "Runs all audit tests loaded into the system."
;;   (rove:run-suite *package*))


;; (defun dump-history-to-file (filename)
;;   "Dumps the in-memory history hash table to a loadable Lisp file."
;;   (let ((filepath (merge-pathnames filename (asdf:system-source-directory :iiscv))))
;;     (format t "Dumping history to ~A...~%" filepath)
;;     (with-open-file (stream filepath
;;                             :direction :output
;;                             :if-exists :supersede)
;;       ;; Escribe la forma para inicializar la tabla hash.
;;       (format stream "(setf *history* (make-hash-table :test 'equal))~%~%")
;;       ;; Itera sobre la tabla hash y escribe cada entrada.
;;       (maphash (lambda (uuid commit-data)
;;                  (format stream "(setf (gethash \"~A\" *history*) '~S)~%"
;;                          uuid
;;                          commit-data))
;;                *history*))))

;; (defun load-history-from-file (filename)
;;   "Loads a history dump file to restore the in-memory history hash table."
;;   (let ((filepath (asdf:system-relative-pathname :iiscv filename)))
;;     (format t "Loading history from ~A...~%" filepath)
;;     (load filepath))
;;   (format t "History loaded successfully.~%"))



;; (defun rebuild-image-from-history ()
;;   "Rebuilds the entire system by evaluating the code from each commit in the history."
;;   (unless *history*
;;     (format t "The history hash table is empty. Please load your history dump file first.~%")
;;     (return-from rebuild-image-from-history nil))

;;   (format t "Rebuilding image from history...~%")
  
;;   ;; This will evaluate the code form for each commit,
;;   ;; effectively re-creating all functions and variables.
;;   (maphash (lambda (uuid commit-data)
;;              (declare (ignore uuid))
;;              (let ((form (second (first (getf commit-data :changes)))))
;;                (format t "Evaluating form from commit with message: ~A~%" (getf commit-data :message))
;;                (eval form)))
;;            *history*)
;;   (format t "Image rebuild completed.~%"))


