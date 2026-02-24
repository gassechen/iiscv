;;;; test-core-final.lisp - Suite Unificada y Definitiva IISCV
(in-package :iiscv)

(format t "~%--- INICIANDO MEGA-SUITE TOTAL E INCREMENTAL ---~%")
(clear-all-commits)
;; =========================================================
;; BLOQUE 1: FUNCIONES Y LÓGICA (ESTILO Y NASA)
;; =========================================================
(make-assert '(defun sum-clean (a b) "Returns the sum of two numbers." (+ a b)))
(make-assert '(defun multiply-no-doc (a b) (* a b)))
(make-assert '(defun unused-param-test (x y) "Solo usa X." (* x x)))
(make-assert '(defun calculate-tax-magic (amount) "Usa constantes literales." (+ (* amount 0.21) 0.15)))
(make-assert '(defun shadow-test (list) "Usa 'list' como variable." (let ((list (car list))) (1+ list))))

;; =========================================================
;; BLOQUE 2: COMPLEJIDAD Y CÓDIGO MUERTO
;; =========================================================
(make-assert '(defun complex-logic-test (x y z)
                (cond ((and (> x 10) (< y 5)) "A")
                      ((or (= x 0) (= y 0)) "B")
                      ((not (null z)) "C")
                      ((> x 100) "D")
                      (t "E"))))

(make-assert '(defun dead-code-test (x) (if t (print "Ok") (print "Inalcanzable"))))

(make-assert '(defun combo-monster (n x)
                (let ((res 0))
                  (setq res (* n 999))
                  (if (> n 10)
                      (if (> x 100) (combo-monster (1- n) x) (print "chico"))
                      (progn (setq res (+ n x 123)) (if t (print "A") (print "B"))))
                  res)))

;; =========================================================
;; BLOQUE 3: ALTA INTEGRIDAD (RECURSIÓN Y LOOPS)
;; =========================================================
(make-assert '(defun factorial-recursive (n) (if (= n 0) 1 (* n (factorial-recursive (1- n))))))
(make-assert '(defun infinite-loop-test (n) (loop (print n) (incf n))))
(make-assert '(defun long-function-no-safety (x y z)
                (let ((res (+ x y z)))
                  (setf res (* res 2)) (setf res (/ res 3))
                  (setf res (+ res 10)) (setf res (- res 5))
                  (setf res (+ res 1)) (setf res (* res 2))
                  (setf res (+ res x)) res)))

(make-assert '(defun silent-fail-test (x) (handler-case (/ 10 x) (error (c) nil))))

;; =========================================================
;; BLOQUE 4: ESTADO GLOBAL (VARIABLES Y CONSTANTES)
;; =========================================================
;; Variables
(make-assert '(defvar *global-state-counter* 0))
(make-assert '(defvar current-user-session nil "Variable mal nombrada."))
(make-assert '(defparameter *tax-rate* 0.21 "IVA estándar."))

;; Constantes (Blindadas con EVAL para evitar abortos en re-load)
(handler-case
    (progn 
      (eval '(defconstant +fixed-rate+ 1.15 "Tasa fija."))
      (make-assert '(defconstant +fixed-rate+ 1.15)))
  (error (c) (format t "~%[AUDIT-INFO] Constant skip: ~A" c)))

(make-assert '(defun spaguetti-state (a b c)
                (setq a (+ a b)) (setq b (* a c)) (setq c (- b a)) (setq a (/ c 2)) (list a b c)))

(make-assert '(defun dynamic-variable-clash (*global-state-counter*) (incf *global-state-counter*)))

;; =========================================================
;; BLOQUE 5: CLOS Y OBJETOS
;; =========================================================
(make-assert '(defclass ghost-class () ((internal-id :initform 0) (raw-data))))
(make-assert '(defmethod break-encapsulation ((obj ghost-class)) (setf (slot-value obj 'internal-id) 999)))
(make-assert '(defclass frankenstein-object (ghost-class) ()))
(make-assert '(defclass system-manager () ((status :initform :idle) (config :initform 42))))

;; =========================================================
;; BLOQUE 6: SEGURIDAD Y HACKS DE SISTEMA
;; =========================================================
(make-assert '(defun security-hole (code) (eval code)))
(make-assert '(defun execute-remote-string (str) (eval (read-from-string str))))

;; Acceso a símbolos internos (Protegido)
(make-assert `(defun hack-internal-logic ()
                (let ((val (or (find-symbol "*HIDDEN*" "IISCV") 100)))
                  (+ val 1))))

;; Errores de exportación manejados
(handler-case (export '(sum-clean non-existent-symbol) :iiscv)
  (error (c) (format t "~%[AUDIT-INFO] Export error handled.~%")))

;; =========================================================
;; BLOQUE 7: PERFORMANCE Y MEMORIA
;; =========================================================
(make-assert '(defun memory-hog (n)
                (let ((acc nil))
                  (loop for i from 1 to n do (setf acc (append acc (list i))))
                  acc)))

(format t "~%--- MEGA-SUITE COMPLETADA SIN ERRORES DE CARGA ---~%")
