;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. REPL & LOADING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun iiscv-repl ()
  "A REPL that automatically commits top-level definition forms and handles errors gracefully."
  (in-package :iiscv)
  (let ((prompt (format nil "~A-R> " (package-name *package*))))
    (loop
      (format t "~%~A" prompt)
      (handler-case
          (let* ((form (read))
                 (form-name (and (listp form) (car form))))
            (cond ((equal form-name 'load)
                   ;; iiscv-load ya se encarga de commitear cada forma, solo llámala.
                   (apply #'iiscv-load (rest form)))
                  (t
                   ;; 1. PRIMERO: Revisa si es una forma que se debe commitear.
                   (when (get-commit-type form)
                     ;; 2. SEGUNDO: Commitea la forma fuente original ANTES de evaluarla.
                     (make-atomic-commit form))
                   ;; 3. TERCERO: Ahora evalúa la forma.
                   (let ((result (eval form)))
                     (unless (eq result :no-print)
                       (print result))))))
        (error (e)
          (format t "~%Error: ~A~%" e)
          (format t "~%Resuming...~%")))))) 




(defun iiscv-load (filename)
  "Loads a .lisp file, auditing and committing each top-level definition."
  (in-package :iiscv)
  (with-open-file (stream filename)
    (loop
      (let ((form (read stream nil :eof)))
        (if (eq form :eof)
            (return)
            (progn
              (when (get-commit-type form)
                (make-atomic-commit form))
              (eval form))))))
  (format t "~%File '~A' loaded and audited successfully.~%" filename))


