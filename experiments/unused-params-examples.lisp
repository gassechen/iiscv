;;; unused-params-examples.lisp
;;; Examples for unused parameters detection

(in-package :iiscv)

;; Test 1: Simple unused parameter
(make-atomic-commit '(defun simple-unused (used unused)
                      (format t "Using: ~A~%" used)
                      (* used 2)))

;; Test 2: Multiple unused parameters
(make-atomic-commit '(defun multi-unused (a b c d e)
                      (format t "Using only: ~A~%" a)
                      (* a 2)))

;; Test 3: Unused parameter in lambda
(make-atomic-commit '(defun lambda-unused (x)
                      (let ((f (lambda (y z) (+ x y))))
                        (funcall f 10 20))))

;; Test 4: Unused parameter with default value
(make-atomic-commit '(defun default-unused (required &optional unused default)
                      (format t "Required: ~A~%" required)
                      required))

;; Test 5: Unused parameter in nested function
(make-atomic-commit '(defun nested-unused (main unused1 unused2)
                      (let ((inner (lambda (unused3)
                                     (format t "Main: ~A~%" main))))
                        (funcall inner 100)
                        main)))