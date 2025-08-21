(defpackage :iiscv
  (:use :cl)
  (:export #:make-commit
           #:get-history
           #:get-commit))

(in-package :iiscv)

;; Global variables to store the history and the current commit state.
(defvar *history* (make-hash-table :test 'equal)
  "Stores commits, with the hash as the key.")

(defvar *current-commit* nil
  "Reference to the hash of the last commit.")

(defun get-function-lambda (function-name)
  "Returns the lambda-form of a named function."
  (ext:macroexpand-all (list 'function function-name)))

(defun make-commit (message function-name)
  "Makes a new commit by getting a function's code and storing it in history."
  (let* ((function-code (get-function-lambda function-name))
         (commit-data
           (list :message message
                 :changes (list (list function-name function-code))
                 :timestamp (get-universal-time)
                 :parent *current-commit*))
         (commit-hash (hash-commit commit-data)))
    (setf (gethash commit-hash *history*) commit-data)
    (setf *current-commit* commit-hash)
    commit-hash))

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
