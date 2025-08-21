(uiop:define-package iiscv
  (:use #:cl)
  (:export #:make-commit
           #:get-history
           #:get-commit
           #:get-last-commit
           #:make-state-snapshot
           #:iiscv-repl
           #:get-commit-form
           #:get-last-uuid-by-name
           #:dump-history-to-file
           #:load-history-from-file
           #:rebuild-image-from-history
           #:run-all-audits))

(in-package #:iiscv)

;; Global variables to store the history and the current commit state.
(defvar *history* (make-hash-table :test 'equal)
  "Stores commits, with the hash as the key.")

(defvar *current-commit* nil
  "Reference to the hash of the last commit.")


(defvar *function-to-uuid-map* (make-hash-table :test 'equal)
  "Maps function names to their last committed UUID.")


;;; Helper function to get the correct documentation type
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
    (t nil)))


(defun get-function-lambda (function-name)
  "Returns the lambda-form of a named function."
  (macroexpand (list 'function function-name)))


(defun make-commit (definition-form)
  "Makes a new commit from any definition form."
  (let* ((name (second definition-form))
         (docstring-type (get-docstring-type definition-form))
         (commit-message (and docstring-type
                              (documentation name docstring-type)))
         (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         (commit-data
          (list :message (or commit-message "No docstring provided for this commit.")
                :changes (list (list name definition-form))
                :timestamp (get-universal-time)
                :parent *current-commit*))
         ;; Construye la clave con el nombre completo.
         (fully-qualified-name (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))
    
    (setf (gethash commit-uuid *history*) commit-data)
    (setf *current-commit* commit-uuid)
    
    ;; Guarda el UUID usando el nombre completo.
    (setf (gethash fully-qualified-name *function-to-uuid-map*) commit-uuid)
    
    (make-file-commit commit-uuid commit-data)
    commit-uuid))


(defun make-file-commit (commit-hash commit-data)
  "Writes a Rove-compatible test file for a commit."
  (let* ((filepath (merge-pathnames (format nil "audits/~A.lisp" commit-hash)
                                    (asdf:system-source-directory :iiscv)))
         (form (second (first (getf commit-data :changes)))))
    (ensure-directories-exist filepath)
    (print filepath) ; <-- Aquí está la línea de diagnóstico
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase))
        (format stream "~S" (make-rove-test-form commit-hash form))))))



(defun make-rove-test-form (commit-hash form)
  "Wraps a Lisp form in a Rove deftest form for auditing."
  `(rove:deftest ,(intern (format nil "COMMIT-~A-TEST" commit-hash) "KEYWORD")
    (rove:ok (eval ',form) "The form should evaluate without error.")))

(defun hash-commit (commit)
  "A placeholder for a cryptographic hash function. Returns a unique string."
  (declare (ignore commit))
  (format nil "~A" (random 100000)))

(defun get-history ()
  "Returns the entire history hash table."
  *history*)

(defun get-commit (commit-hash)
  "Returns a commit object given its hash."
  (gethash commit-hash *history*))

(defun get-commit-form (commit-uuid)
  "Retrieves the original Lisp form from a commit in the history."
  (let* ((commit-data (gethash commit-uuid *history*)))
    (unless commit-data
      (format t "Error: Commit with UUID ~A not found in history.~%" commit-uuid)
      (return-from get-commit-form nil))
    
    (second (first (getf commit-data :changes)))))


(defun get-last-commit ()
  "Returns the hash of the last commit."
  (get-commit *current-commit*))



(defun make-state-snapshot (symbol-list &key (message "Snapshot de estado de variables."))
  "Takes a snapshot of the current state of a list of symbols and creates a commit."
  (let ((state-forms '()))
    (dolist (sym symbol-list)
      (when (boundp sym)
        (let* ((current-value (symbol-value sym))
               (form `(setf ,sym ',current-value)))
          (push form state-forms))))
    
    ;; Ahorraremos el snapshot como una forma de Lisp
    (let* ((snapshot-data `(progn ,@state-forms))
           (commit-data
             (list :message message
                   :changes (list (list :snapshot snapshot-data))
                   :timestamp (get-universal-time)
                   :parent *current-commit*))
           (commit-hash (hash-commit commit-data)))
      (setf (gethash commit-hash *history*) commit-data)
      (setf *current-commit* commit-hash)
      (make-file-commit commit-hash commit-data)
      commit-hash)))

(defun browse-history ()
  "Displays all commits stored in the in-memory history."
  (format t "~%--- History ---~%")
  (maphash (lambda (hash commit)
             (format t "Commit: ~A~%" hash)
             (format t "  Parent: ~A~%" (getf commit :parent))
             (format t "  Timestamp: ~A~%" (getf commit :timestamp))
             (format t "  Message: ~A~%~%" (getf commit :message)))
           *history*))


(defun get-last-uuid-by-name (name-symbol)
  "Returns the UUID of the last committed version of a function by its name."
  (let* ((package-name (package-name (symbol-package name-symbol)))
         (symbol-name (symbol-name name-symbol))
         (search-key (format nil "~A::~A" package-name symbol-name)))
    (gethash search-key *function-to-uuid-map*)))



(defun iiscv-repl ()
  "A REPL that automatically commits top-level definition forms."
  (in-package :iiscv)
  (let ((prompt (format nil "~A-R> " (package-name *package*))))
    (loop
      (format t "~%~A" prompt)
      (let* ((form (read))
	     (doc-type (get-docstring-type form)))
	(let ((result (eval form))) ; <-- Evaluar la forma primero
	  (unless (eq result :no-print)
	    (print result)))
	(when doc-type
	  (print (make-commit form))))))) 


(defun run-all-audits ()
  "Runs all audit tests loaded into the system."
  (rove:run-suite *package*))


(defun dump-history-to-file (filename)
  "Dumps the in-memory history hash table to a loadable Lisp file."
  (let ((filepath (merge-pathnames filename (asdf:system-source-directory :iiscv))))
    (format t "Dumping history to ~A...~%" filepath)
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :supersede)
      ;; Escribe la forma para inicializar la tabla hash.
      (format stream "(setf *history* (make-hash-table :test 'equal))~%~%")
      ;; Itera sobre la tabla hash y escribe cada entrada.
      (maphash (lambda (uuid commit-data)
                 (format stream "(setf (gethash \"~A\" *history*) '~S)~%"
                         uuid
                         commit-data))
               *history*))))

(defun load-history-from-file (filename)
  "Loads a history dump file to restore the in-memory history hash table."
  (let ((filepath (asdf:system-relative-pathname :iiscv filename)))
    (format t "Loading history from ~A...~%" filepath)
    (load filepath))
  (format t "History loaded successfully.~%"))



(defun rebuild-image-from-history ()
  "Rebuilds the entire system by evaluating the code from each commit in the history."
  (unless *history*
    (format t "The history hash table is empty. Please load your history dump file first.~%")
    (return-from rebuild-image-from-history nil))

  (format t "Rebuilding image from history...~%")
  
  ;; This will evaluate the code form for each commit,
  ;; effectively re-creating all functions and variables.
  (maphash (lambda (uuid commit-data)
             (declare (ignore uuid))
             (let ((form (second (first (getf commit-data :changes)))))
               (format t "Evaluating form from commit with message: ~A~%" (getf commit-data :message))
               (eval form)))
           *history*)
  (format t "Image rebuild completed.~%"))


