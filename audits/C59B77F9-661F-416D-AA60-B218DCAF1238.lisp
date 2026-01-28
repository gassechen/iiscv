(rove/core/test:deftest :commit-c59b77f9-661f-416d-aa60-b218dcaf1238-test
  (rove/core/assertion:ok
   (eval
    '(defpackage :ioe-app
       (:use :cl :iiscv)
       (:documentation "Paquete de aplicación industrial para operaciones de persistencia")))
   "The form should evaluate without error."))