(rove/core/test:deftest :commit-f3619adf-c64f-4b96-9558-75f88015f633-test
  (rove/core/assertion:ok
   (eval
    '(defpackage :ioe-core
       (:use :cl :iiscv)
       (:documentation "Core package for IOE development with IISCV integration")))
   "The form should evaluate without error."))