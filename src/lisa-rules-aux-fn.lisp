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

