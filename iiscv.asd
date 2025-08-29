(defsystem "iiscv"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("rove" "uuid" "cl-graph" "lisa" "external-program") 
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "lisa-rules")
		 (:file "lisa-rules-aux-fn"))))
  :description ""
  :in-order-to ((test-op (test-op "iiscv/tests"))))

