;; audits-run.lisp
(in-package #:iiscv)
(defun run-all-audits ()
  "Loads and runs all audit files in the audits directory."
  (let ((audit-files (directory (asdf:system-relative-pathname "iiscv" "audits/*.lisp"))))
    (unless audit-files
      (format t "No audit files found in ~A~%"
	      (asdf:system-relative-pathname "iiscv" "audits/"))
      (return-from run-all-audits nil))

    (format t "Running ~A audit files...~%" (length audit-files))
    (loop for file in audit-files do
      (load file))
    (rove:run :package :audits)))
