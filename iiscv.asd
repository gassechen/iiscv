(defsystem "iiscv"
  :version "0.0.1"
  :author "gassechen"
  :license "BSD"
  :depends-on ("rove" "uuid" "cl-graph" "lisa" "external-program" "lisp-critic") 
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "utility-fn")
		 (:file "make-human-commits")
		 (:file "make-class-commits")
		 (:file "repl-r")
		 (:file "dump-source-code")
		 (:file "make-rove-test")
		 (:file "register-commit-type")
		 (:file "reports")
		 (:file "lisa-rules")
		 (:file "lisa-rules-aux-fn"))))
  :description ""
  :in-order-to ((test-op (test-op "iiscv/tests"))))

