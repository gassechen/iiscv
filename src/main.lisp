(uiop:define-package iiscv
  (:use #:cl #:LISA-LISP)
  (:shadowing-import-from #:LISA-LISP #:assert) 
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
  "Creates a new atomic commit and extracts all the data for the quality audit."
  (setq *audit-violations* nil)
  (lisa:reset)

  (let* ((name (and (listp definition-form) (second definition-form)))
         (docstring (get-docstring definition-form))
         (has-docstring-p (not (null docstring)))
         (body-length (calculate-body-length definition-form))
         (cyclomatic-complexity (calculate-cyclomatic-complexity definition-form))
         (magic-numbers (find-magic-numbers definition-form))
         (unused-parameters (find-unused-parameters definition-form))
         (is-redefining-core-symbol-p (is-redefining-core-symbol-p name))
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

      (when name
        (let ((fully-qualified-name (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))))
          (setf (gethash fully-qualified-name *function-to-uuid-map*) commit-uuid)))

      (make-file-commit commit-uuid definition-form)

      (format t "~%Violations detected: ~A~%" (length *audit-violations*))
      (format t "~{~a~%~}" (mapcar #'car *audit-violations*))
      commit-uuid)))




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


(defun find-magic-numbers (form)
  "Recursively traverses a form to find literal numbers
   other than 0 or 1 and returns a list of them."
  (let ((found-numbers nil))
    (labels ((scan (subform)
               (cond ((listp subform)
                      (dolist (item subform)
                        (scan item)))
                     ((and (numberp subform)
                           (not (member subform '(0 1))))
                      (push subform found-numbers)))))
      (scan form)
      (reverse found-numbers))))


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



(defun find-implementation-specific-symbols (form)
  "Recursively finds symbols that are not in a standard Common Lisp package."
  (let ((found-symbols nil)
        (standard-packages '(:common-lisp :keyword :cl-user :lisp :editor)))
    (labels ((scan (subform)
               (cond ((atom subform)
                      (when (and (symbolp subform)
                                 (not (keywordp subform)))
                        (let* ((pkg (symbol-package subform))
                               (pkg-name (and pkg (package-name pkg))))
                          (when (and pkg
                                     (not (member (intern (string-upcase pkg-name) :keyword)
                                                  standard-packages)))
                            (push subform found-symbols)))))
                     ((listp subform)
                      (dolist (item subform)
                        (scan item))))))
      (scan (cddr form))
      (not (null found-symbols)))))
      



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


(defun get-parameters-list (definition-form)
  "Extracts the parameters list from a definition form."
  (when (and (listp definition-form)
             (member (car definition-form) '(defun defmacro)))
    (third definition-form)))



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
                                 (not (keywordp subform)))
                        (incf (gethash subform used-symbols 0))))
                     ((listp subform)
                      (dolist (item subform)
                        (scan item))))))
      (scan form)
      used-symbols)))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *human-history-graph*
  (make-instance 'cl-graph:dot-graph)
  "Graph to store high-level, human-readable commits. The history for humans.")

(defvar *current-human-commit* nil
  "Reference to the UUID of the last human-level commit.")


(defun human-commit (message symbols &optional (file-path nil))
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
    ;;agregar para volvar las funciones a archivos opcional
    ;;(when file-path
    ;;  (let ((full-path (merge-pathnames (format nil "src/~A" file-path)
    ;;                                     (asdf:system-source-directory :iiscv))))
    ;;    (ensure-directories-exist full-path)
    ;;    (with-open-file (stream full-path :direction :output :if-exists :supersede)
    ;;      (format t "~%Writing human commit to file: ~A~%" full-path)
    ;;      (dolist (atomic-uuid atomic-uuids)
    ;;        (let* ((atomic-vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
    ;;               (atomic-data (when atomic-vertex (cl-graph:element atomic-vertex))))
    ;;          (when atomic-data
    ;;            (let ((source-form (getf atomic-data :source-form)))
    ;;              (format stream "~%~S~%~%" source-form))))))))
    ;; (format t "~%New human commit created with UUID: ~A~%" human-uuid)
    ;;(human-commit "Added user authentication module"
    ;;          '(make-db-connection check-password-validity authenticate-user)
    ;;          "user-auth.lisp")

    
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

(defvar *commit-type-registry* (make-hash-table :test 'equal)
  "A registry for associating Lisp forms with commit types.")

(defun register-commit-type (form-name commit-type)
  "Registers a new Lisp form name and its associated commit type."
  (setf (gethash form-name *commit-type-registry*) commit-type))

(defun get-docstring-type (form)
  "Returns the documentation type for a given definition form from the registry."
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
;;     (ql:quickload 'dependency) 
;;     (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Granular class actions

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
          (format t "  Timestamp: ~A~%" (getf data :timestamp))
          (format t "  Violations detected: ~A~%" (mapcar #'car (getf data :rules-violations))))))))



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

