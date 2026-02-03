;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9. ROVE & AUDITS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun make-rove-test-form (commit-uuid form)
  "Wraps a Lisp form in a Rove deftest form for auditing."
  `(rove:deftest ,(intern (format nil "COMMIT-~A-TEST" commit-uuid) "KEYWORD")
     (rove:ok (eval ',form) "The form should evaluate without error.")))

;; path de proyecto
(defun make-file-commit (commit-uuid form)
  "Writes a Rove-compatible test file for a commit."
  (let* ((filepath (merge-pathnames (format nil "audits/~A.lisp" commit-uuid)
                                    (asdf:system-source-directory :iiscv))))
    (ensure-directories-exist filepath)
    (with-open-file (stream filepath
			    :direction
			    :output
			    :if-exists
			    :supersede)
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

