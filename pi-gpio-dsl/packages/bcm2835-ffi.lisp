;;;; bcm2835-ffi.lisp
;; FFI para controlar GPIO en Raspberry Pi (chip BCM2835)

(in-package #:cl-user)

(defpackage #:gpio-ffi
  (:use #:cl #:cffi)
  (:export #:gpio-init
           #:gpio-set-direction
           #:gpio-set
           #:gpio-clear
           #:gpio-write
           #:delay-microseconds
           #:input
           #:output))

(cl:in-package #:gpio-ffi)

;; Definiciones de constantes y tipos para BCM2835

(define-foreign-library libbcm2835
  (:t "libbcm2835.so"))

(use-foreign-library libbcm2835)

;; Constantes de dirección
(defcenum gpio-direction
  :input 0
  :output 1)

;; Funciones de libbcm2835
(defcfun ("bcm2835_init" gpio-init) :int)

(defcfun ("bcm2835_gpio_fsel" gpio-set-direction) :void
  (pin :unsigned-int)
  (mode gpio-direction))

(defcfun ("bcm2835_gpio_set" gpio-set) :void
  (pin :unsigned-int))

(defcfun ("bcm2835_gpio_clr" gpio-clear) :void
  (pin :unsigned-int))

(defcfun ("bcm2835_gpio_write" gpio-write) :void
  (pin :unsigned-int)
  (on :unsigned-int))

(defcfun ("bcm2835_delayMicroseconds" delay-microseconds) :void
  (micros :unsigned-int))

;; Wrapper Lisp seguros con auditoría

(defun safe-gpio-init ()
  "Inicializa el sistema GPIO con auditoría de IISCV."
  (let ((result (gpio-init)))
    (iiscv:make-atomic-commit
     '(defun gpio-init ()
        "Inicializa el sistema BCM2835"
        (gpio-ffi:gpio-init)))
    (when (= result -1)
      (error "Failed to initialize GPIO"))
    result))

(defun safe-gpio-set-direction (pin direction)
  "Configura dirección de un pin con auditoría de IISCV."
  (iiscv:make-atomic-commit
   `(defun gpio-set-direction (,pin ,direction)
      ,(format nil "Configura pin ~A como ~A" pin direction)
      (gpio-ffi:gpio-set-direction ,pin ,direction)))
  (gpio-ffi:gpio-set-direction pin direction))

(defun safe-gpio-write (pin state)
  "Escribe estado a un pin con auditoría de IISCV."
  (iiscv:make-atomic-commit
   `(defun gpio-write (,pin ,state)
      ,(format nil "Escribe estado ~A al pin ~A" state pin)
      (gpio-ffi:gpio-write ,pin state)))
  (gpio-ffi:gpio-write pin state))
