;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10. TYPE REGISTRY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defvar *commit-type-registry* (make-hash-table :test 'equal))

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
(register-commit-type 'add-slot 'slot-change)
(register-commit-type 'remove-slot 'slot-change)
(register-commit-type 'ql:quickload 'dependency)

