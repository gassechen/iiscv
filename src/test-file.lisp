;; test-file.lisp
(in-package :iiscv)

(defun test-function ()
  "This is a test function to be audited."
  (format t "Hello from a loaded file!"))

(defvar *test-variable* 100)
