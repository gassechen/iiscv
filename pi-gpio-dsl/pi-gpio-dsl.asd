;;;; pi-gpio-dsl.asd
;; Sistema para control de GPIO en Raspberry Pi usando IISCV

(asdf:defsystem "pi-gpio-dsl"
  :description "DSL para controlar pines GPIO de Raspberry Pi con auditoría IISCV"
  :version "0.1.0"
  :author "Industrial Lisp Machine Project"
  :license "MIT"
  :depends-on ("iiscv" "cffi")
  :components
  ((:module "packages"
    :components
    ((:file "iiscv")
     (:file "bcm2835-ffi")
     (:file "gpio-dsl")
     (:file "gpio-app")))
   (:module "drivers"
    :components
    ((:file "bcm2835-ffi")))))
