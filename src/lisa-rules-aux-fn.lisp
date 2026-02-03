;;; lisa-rules-aux-fn.lisp
;;; Forensic Analysis Auxiliary Functions for IISCV

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. BODY & METRICS SENSORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-body-forms (definition-form)
  "Extracts the body from a definition form, correctly handling docstrings."
  (cond ((and (listp definition-form)
              (member (car definition-form) '(defun defmacro)))
         (let ((body (cdddr definition-form)))
           (if (and (listp body) (stringp (car body)))
               (cdr body)
               body)))
        (t nil)))

(defun calculate-body-length (definition-form)
  "Calculates the approximate length of a function's body by counting top-level forms."
  (let ((body-forms (get-body-forms definition-form)))
    (if body-forms (length body-forms) 0)))

(defun count-decision-points (form)
  "Recursively traverses a form to count control structures that increase cyclomatic complexity."
  (let ((count 0))
    (when (listp form)
      (case (car form)
        ((if when unless cond case loop dolist dotimes)
         (incf count)))
      (dolist (subform (cdr form))
        (incf count (count-decision-points subform))))
    count))

(defun calculate-cyclomatic-complexity (definition-form)
  "Calculates the cyclomatic complexity (McCabe) of a function."
  (let ((body (and (listp definition-form) (cddr definition-form))))
    (when (and body (stringp (car body)))
      (setf body (cdr body)))
    (+ 1 (count-decision-points body))))

(defun find-magic-numbers (form)
  "Finds and returns a list of magic numbers in a Lisp form, excluding constants and variables."
  (let ((found-numbers nil))
    (labels ((scan (subform)
               (cond ((numberp subform)
                      (unless (member subform '(0 1)) (push subform found-numbers)))
                     ((listp subform)
                      (let ((form-type (car subform)))
                        (unless (member form-type '(defconstant defvar defparameter quote))
                          (dolist (item (cdr subform)) (scan item))))))))
      (scan form)
      (when found-numbers (list (remove-duplicates found-numbers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. PARAMETERS & USAGE ANALYSIS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-parameters-list (definition-form)
  "Extracts the parameters list from a definition form."
  (when (and (listp definition-form)
             (member (car definition-form) '(defun defmacro)))
    (let ((params (third definition-form)))
      (extract-parameter-names params))))

(defun extract-parameter-names (params)
  "Extracts only parameter names, ignoring keywords and initial values."
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
  "Finds parameters in a definition that are declared but not used in the body."
  (let* ((params (get-parameters-list definition-form))
         (body (get-body-forms definition-form))
         (used-symbols (find-used-symbols body))
         (unused-params nil))
    (setf params (remove-if (lambda (param) 
                              (member param '(&optional &rest &key &aux 
                                               &allow-other-keys &whole &environment)))
                            params))
    (dolist (param params)
      (when (and (symbolp param) (not (gethash param used-symbols)))
        (push param unused-params)))
    (reverse unused-params)))

(defun find-used-symbols (form)
  "Recursively finds and counts all symbols used in a form."
  (let ((used-symbols (make-hash-table :test 'eq)))
    (labels ((scan (subform)
               (cond ((atom subform)
                      (when (and (symbolp subform)
                                 (not (keywordp subform))
                                 (not (member subform '(&optional &rest &key &aux 
                                                            &allow-other-keys &whole &environment))))
                        (incf (gethash subform used-symbols 0))))
                     ((listp subform)
                      (dolist (item subform) (scan item))))))
      (scan form))
    used-symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. SAFETY & LOGIC SENSORS (NASA Power of Ten)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-docstring (definition-form)
  "Extracts the docstring from a definition form, returning NIL if none exists."
  (let ((docstring-candidate (nthcdr 2 definition-form)))
    (loop for form in docstring-candidate
          when (stringp form) return form
          when (and (listp form) (not (eq (car form) 'declare))) return nil)))

(defun has-dead-code-p (form)
  "Detects unreachable logic branches based on constant predicates."
  (let ((found nil))
    (labels ((scan (x)
               (cond ((listp x)
                      (case (car x)
                        ((if when unless)
                         (when (member (second x) '(t nil)) (setf found t)))
                        (cond (dolist (c (cdr x))
                                (when (member (car c) '(t nil)) (setf found t)))))
                      (mapc #'scan (cdr x))))))
      (scan form)
      found)))

(defun is-recursive-p (name form)
  "NASA Rule 1: Correctly detects if the function calls itself."
  (let* ((calls (extract-calls form name)) ;; <--- IMPORTANTE: pasar 'name'
         (fqn (if (symbolp name)
                  (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name))
                  (princ-to-string name))))
    (member fqn calls :test #'equal)))


(defun count-assertions (form)
  "NASA Rule 5: Counts defensive mechanisms (assert, check-type, error)."
  (let ((count 0)
        (check-ops '(assert check-type error warn ecase etypecase))
        (flat-form (alexandria:flatten form)))
    (dolist (item flat-form)
      (when (member item check-ops) (incf count)))
    count))

(defun has-unbounded-loops-p (form)
  "NASA Rule 2: Detects potentially infinite loops (LOOP without exit clauses)."
  (let ((found-unbounded nil))
    (labels ((scan (x)
               (cond ((listp x)
                      (when (eq (car x) 'loop)
                        (unless (intersection (alexandria:flatten x) '(repeat for while until))
                          (setf found-unbounded t)))
                      (mapc #'scan x)))))
      (scan form)
      found-unbounded)))

(defun extract-mutated-symbols (form)
  "Recursively detects state mutation (setf, setq, etc.)."
  (let ((mutated '())
        (ops '(setf setq incf decf push pop nconc)))
    (labels ((scan (x)
               (cond ((listp x)
                      (when (member (car x) ops)
                        (pushnew (format nil "~A" (second x)) mutated :test #'equal))
                      (mapc #'scan (cdr x))))))
      (scan form)
      mutated)))



(defun is-lisp-predicate-p (name)
  "Convention: Checks if a function name follows the predicate naming convention (-P)."
  (let ((n (string-upcase (princ-to-string name))))
    (and (> (length n) 2) (string= "-P" (subseq n (- (length n) 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. SYSTEM & STYLE SENSORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-unsafe-execution-forms (form)
  "Finds forms associated with external command execution."
  (let ((unsafe-symbols '(uiop:run-program external-program:start))
        (found '()))
    (labels ((scan (x)
               (when (listp x)
                 (when (member (car x) unsafe-symbols) (push x found))
                 (mapc #'scan (cdr x)))))
      (scan form)
      found)))

(defun find-implementation-specific-symbols (form)
  "Finds symbols belonging to implementation-specific packages."
  (let ((found nil)
        (standard-packages '("COMMON-LISP" "KEYWORD" "IISCV" "CL-USER")))
    (labels ((scan (subform)
               (cond ((atom subform)
                      (when (and (symbolp subform) (not (keywordp subform)))
                        (let* ((pkg (symbol-package subform))
                               (pkg-name (and pkg (package-name pkg))))
                          (when (and pkg (not (member pkg-name standard-packages :test #'string-equal)))
                            (setf found t)))))
                     ((listp subform) (dolist (item subform) (scan item))))))
      (scan form)
      found)))

(defun contains-heavy-consing-loop-p (definition-form)
  "Checks for allocation (consing) inside loops."
  (let ((detected nil)
        (cons-ops '(cons list list* make-array make-hash-table)))
    (labels ((scan (f)
               (when (and (listp f) (not detected))
                 (if (member (car f) '(loop dolist dotimes))
                     (let ((flat-body (alexandria:flatten f)))
                       (when (intersection flat-body cons-ops) (setf detected t)))
                     (mapc #'scan (cdr f))))))
      (scan definition-form)
      detected)))

(defun clean-critic-report (raw-report)
  "Removes decorative dash lines and white space from lisp-critic."
  (let* ((no-dashes (cl-ppcre:regex-replace-all "-{3,}" raw-report ""))
         (clean-text (string-trim '(#\Space #\Newline #\Tab) no-dashes)))
    (if (plusp (length clean-text)) clean-text nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. DEPENDENCY & GRAPH SENSORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun extract-calls (form &optional current-name)
  "Extracts calls, including the function itself (for recursion detection)."
  (let ((calls '())
        (current-fqn (when current-name 
                       (if (symbolp current-name)
                           (format nil "~A::~A" (package-name (symbol-package current-name)) (symbol-name current-name))
                           (princ-to-string current-name)))))
    (labels ((scan (x)
               (cond ((listp x)
                      (let ((head (car x)))
                        (when (symbolp head)
                          (let ((fqn (format nil "~A::~A" (package-name (symbol-package head)) (symbol-name head))))
                            (when (or (gethash fqn iiscv::*function-to-uuid-map*)
                                      (string= fqn current-fqn))
                              (pushnew fqn calls :test #'equal))))
                        (mapc #'scan x))))))
      (scan (get-body-forms form))
      calls)))



(defun find-dependents-in-history (function-name)
  "Searches for active functions that depend on the given function-name."
  (let ((affected nil)
        (target-str (if (symbolp function-name)
                        (format nil "~A::~A" (package-name (symbol-package function-name)) (symbol-name function-name))
                        (princ-to-string function-name))))
    (when (and (boundp 'iiscv::*atomic-history-graph*) iiscv::*atomic-history-graph*)
      (cl-graph:iterate-vertexes iiscv::*atomic-history-graph*
        (lambda (v)
          (let ((vertex-data (cl-graph:element v)))
            (when (listp vertex-data)
              (let* ((s-name (getf vertex-data :symbol-name))
                     (active-uuid (gethash (if (symbolp s-name) 
                                               (format nil "~A::~A" (package-name (symbol-package s-name)) (symbol-name s-name))
                                               (princ-to-string s-name)) 
                                           *function-to-uuid-map*)))
                (when (and (equal (getf vertex-data :UUID) active-uuid)
                           (member target-str (getf vertex-data :calls) :test #'equal))
                  (push s-name affected))))))))
    (delete-duplicates affected :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. LISA AUDITOR BRIDGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun analyze-commit-and-assert (&key uuid name has-docstring-p body-length
				    cyclomatic-complexity magic-numbers
				    unused-parameters is-redef calls
				    uses-unsafe-execution-p contains-heavy-consing-loop-p
				    uses-implementation-specific-symbols-p style-critiques status
                                    ;; NASA & Logic keys
                                    is-predicate has-dead-code is-recursive 
                                    assertion-count has-unbounded-loop
                                    has-side-effects returns-constant-nil)
  "Feeds all forensic data into the LISA engine."
  (setq *audit-violations* nil)
  (lisa:reset)
  (let ((fact-data
          `(code-commit-analysis
            (commit-uuid ,uuid)
            (symbol-name ,name)
            (body-length ,body-length)
            (cyclomatic-complexity ,cyclomatic-complexity)
            (magic-numbers ',magic-numbers)
            (is-redefining-core-symbol-p ,is-redef)
            (uses-unsafe-execution-p ,uses-unsafe-execution-p)
            (contains-heavy-consing-loop-p ,contains-heavy-consing-loop-p)
            (has-docstring-p ,has-docstring-p)
            (unused-parameters ',unused-parameters)
            (uses-implementation-specific-symbols-p ,uses-implementation-specific-symbols-p)
	    (style-critiques ,style-critiques)
	    (calls ',calls)
            (status ,status)
            ;; NASA & Logic mapping
            (is-predicate-p ,is-predicate)
            (has-dead-code-p ,has-dead-code)
            (is-recursive-p ,is-recursive)
            (assertion-count ,assertion-count)
            (has-unbounded-loop-p ,has-unbounded-loop)
            (has-side-effects-p ,has-side-effects)
            (returns-constant-nil-p ,returns-constant-nil))))
    (eval `(lisa:assert ,fact-data)))
  (lisa:run)
  (lisa:run))
