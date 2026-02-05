;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. SEARCH & UTILITY (RESTORED)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

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



(defun get-source-form-by-uuid (uuid)
  "Retrieves the source form of a commit by its UUID."
  (let* ((vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
         (commit-data (when vertex (cl-graph:element vertex))))
    (if commit-data
        (getf commit-data :source-form)
        (format t "Error: Commit with UUID ~A not found in atomic history.~%" uuid))))



(defun get-last-uuid-by-name (name-symbol)
  "Returns the latest active UUID for a given symbol."
  (let ((key (if (symbolp name-symbol)
                 (format nil "~A::~A" (package-name (symbol-package name-symbol)) (symbol-name name-symbol))
                 (princ-to-string name-symbol))))
    (gethash key *function-to-uuid-map*)))



(defun find-vertex-by-uuid (graph uuid)
  "Finds and returns a vertex in a graph by its UUID property."
  (let ((found nil))
    (cl-graph:iterate-vertexes graph
       (lambda (v)
         (let ((data (cl-graph:element v)))
           (when (and (listp data) (string= (getf data :UUID) uuid))
             (setf found v)))))
    found))


(defun get-data-from-vertex (vertex)
  "Safely retrieves the data list from a cl-graph vertex."
  (let ((element (and vertex (cl-graph:element vertex))))
    (when (listp element) element)))


(defun find-vertex-by-symbol-name (function-name)
  "Find the vertex of the active version of a function in the atomic graph."

  (let* ((fqn (if (symbolp function-name)
		  (format nil "~A::~A" (package-name (symbol-package function-name)) (symbol-name function-name))
		  (princ-to-string function-name)))
	 (uuid (gethash fqn *function-to-uuid-map*)))
    (if uuid
	(find-vertex-by-uuid *atomic-history-graph* uuid)
	nil)))



;; VERR OJO
;;(defun get-last-uuid-by-name (name-symbol)
;;  "Returns the UUID of the last committed version of a function by its name."
;;  (let* ((package-name (package-name (symbol-package name-symbol)))
;;         (symbol-name (symbol-name name-symbol))
;;         (search-key (format nil "~A::~A" package-name symbol-name)))
;;    (gethash search-key *function-to-uuid-map*)))


(defun string-join (list-of-strings separator)
  "Joins a list of strings with a separator."
  (format nil (format nil "~~{~~a~~^~a~~}" separator) list-of-strings))
