(defsystem "iiscv"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("rove" "uuid" "cl-graph") 
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "iiscv/tests"))))

