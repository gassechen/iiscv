;; audits.asd
(defsystem "audits"
  :author "The IISCV System"
  :depends-on ("rove")
  :components `((:module "audits"
                 :components ,(loop for file in (directory "audits/*.lisp")
                                    collect `(:file ,(pathname-name file)))))
  :perform (test-op (op c) (symbol-call :rove :run :package :audits)))
