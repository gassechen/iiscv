(defsystem "iiscv/tests"
  :author "Tu Nombre"
  :license "Tu Licencia"
  :depends-on ("iiscv"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for iiscv"
  :perform (test-op (op c) (symbol-call :rove :run c)))
