;;;; gpio-dsl.lisp
;; DSL de alto nivel para controlar GPIO en Raspberry Pi
;; con auditoría especializada para seguridad industrial

(in-package #:cl-user)

(defpackage #:gpio-dsl
  (:use #:cl #:iiscv #:gpio-ffi)
  (:shadowing-import-from #:iiscv #:assert)
  (:export #:def-pin
           #:def-led-control
           #:def-button-sensor
           #:def-relay
           #:with-gpio-safe
           #:start-gpio-system
           #:stop-gpio-system
           #:get-last-pin-configuration))

(cl:in-package #:gpio-dsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables Globales del DSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *active-pins* (make-hash-table :test 'equal)
  "Hash table que mantiene el estado de todos los pines activos.")

(defvar *pin-initialization-order* nil
  "Lista ordenada de inicialización de pines para rollback seguro.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motor de Inferencia LISA para DSL GPIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lisa:make-inference-engine)

(deftemplate pin-configuration ()
  (slot pin-number)
  (slot pin-name)
  (slot pin-mode)
  (slot pin-state)
  (slot last-activation)
  (slot activation-frequency)
  (slot is-critical))

(deftemplate gpio-violation ()
  (slot rule-id)
  (slot severity)
  (slot message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reglas de Auditoría para DSL GPIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Regla 1: No permitir pines reservados del sistema
(defrule gpio-rule-1-reserved-pins ()
  (pin-configuration (pin-number ?num)
                   (pin-name ?name))
  (test (member ?num '(2 3)))
  =>
  (assert (gpio-violation (rule-id "GPIO-1")
                        (severity :error)
                        (message (format nil "Pin ~A (~A) está reservado para I2C. El pin 2 es SDA y el 3 es SCL."
                                        ?num ?name)))))

;; Regla 2: Prevenir conflictos de dirección en el mismo pin
(defrule gpio-rule-2-direction-conflict ()
  (pin-configuration (pin-number ?num)
                   (pin-mode ?mode1))
  (pin-configuration (pin-number ?num)
                   (pin-mode ?mode2))
  (test (not (eq ?mode1 ?mode2)))
  =>
  (assert (gpio-violation (rule-id "GPIO-2")
                        (severity :error)
                        (message (format nil "Conflicto de dirección en pin ~A: ~A vs ~A"
                                        ?num ?mode1 ?mode2)))))

;; Regla 3: Advertencia de activación frecuente en salidas críticas
(defrule gpio-rule-3-critical-frequency ()
  (pin-configuration (pin-number ?num)
                   (pin-name ?name)
                   (is-critical t)
                   (activation-frequency ?freq))
  (test (> ?freq 100))
  =>
  (assert (gpio-violation (rule-id "GPIO-3")
                        (severity :warning)
                        (message (format nil "Pin ~A (~A) crítico activándose con alta frecuencia (~A Hz)"
                                        ?num ?name ?freq)))))

;; Regla 4: Seguridad en relés (no permitir conmutación sin delay)
(defrule gpio-rule-4-relay-safety ()
  (pin-configuration (pin-number ?num)
                   (pin-name ?name))
  (test (search "RELAY" (string ?name)))
  (pin-configuration (pin-number ?num)
                   (last-activation ?time1))
  (pin-configuration (pin-number ?num)
                   (last-activation ?time2))
  (test (< (- ?time2 ?time1) 10))
  =>
  (assert (gpio-violation (rule-id "GPIO-4")
                        (severity :error)
                        (message (format nil "Relé ~A (pin ~A) conmutando sin delay mínimo (10ms)"
                                        ?name ?num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros del DSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-pin (name number mode &key (critical nil) (initial-state 0))
  "Define un pin GPIO con auditoría de seguridad.

   NAME: Nombre simbólico del pin
   NUMBER: Número físico del pin BCM2835
   MODE: :input o :output
   CRITICAL: Si es T, el pin se monitorea por activaciones rápidas
   INITIAL-STATE: Estado inicial (0 o 1)"

  (let ((pin-var (intern (format nil "*PIN-~A*" (string-upcase name))))
    `(progn
       ;; 1. Registrar configuración en LISA
       (iiscv:make-atomic-commit
        '(defun ,name ()
           ,(format nil "Pin GPIO ~A: ~A en modo ~A~@[, crítico~]"
                    number name mode critical)
           (gpio-ffi:gpio-set-direction ,number ,mode)))

       ;; 2. Asertar hecho en motor LISA
       (lisa:assert (pin-configuration
                      (pin-number ,number)
                      (pin-name ',name)
                      (pin-mode ,mode)
                      (pin-state ,initial-state)
                      (last-activation 0)
                      (activation-frequency 0)
                      (is-critical ,critical)))

       ;; 3. Ejecutar análisis de reglas
       (lisa:run)

       ;; 4. Verificar violaciones
       (when gpio-violations
         (format t "~%~%ERROR: Configuración de pin rechazada por auditoría GPIO:")
         (dolist (violation gpio-violations)
           (format t "~%  [~A] ~A"
                   (getf violation :rule-id)
                   (getf violation :message)))
         (error "Violación de auditoría GPIO. No se puede crear el pin."))

       ;; 5. Crear variable global para el pin
       (defvar ,pin-var ,initial-state)
       (setf (gethash ',name *active-pins*) ,pin-var)
       (push (list ',name ,number) *pin-initialization-order*)

       (defun ,(intern (format nil "SET-~A" name)) (state)
         ,(format nil "Establece el estado del pin ~A con auditoría" name)
         (iiscv:make-atomic-commit
          `(setf ,',pin-var ,state))
         (gpio-ffi:gpio-write ,number state)
         (when (eq ,mode :output)
           (update-pin-statistics ',name state))
         state))))

(defmacro def-led-control (name pin &key (blink-interval 100))
  "Define un LED controlable con capacidad de parpadeo.

   NAME: Nombre del LED
   PIN: Número de pin
   BLINK-INTERVAL: Intervalo en milisegundos para parpadeo"

  `(progn
     (def-pin ,name ,pin :mode :output :initial-state 0)

     (defun ,(intern (format nil "BLINK-~A" name)) (&optional (times 1))
       ,(format nil "Parpadea el LED ~A ~A veces" name times)
       (dotimes (i times)
         (,(intern (format nil "SET-~A" name)) 1)
         (delay-microseconds ,blink-interval)
         (,(intern (format nil "SET-~A" name)) 0)
         (delay-microseconds ,blink-interval)))))

(defmacro def-button-sensor (name pin &key (debounce-time 50))
  "Define un botón sensor con anti-rebote.

   NAME: Nombre del botón
   PIN: Número de pin
   DEBOUNCE-TIME: Tiempo de debounce en milisegundos"

  `(progn
     (def-pin ,name ,pin :mode :input)

     (defvar ,(intern (format nil "*~A-LAST-STATE*" name)) 0)

     (defun ,(intern (format nil "READ-~A" name)) ()
       ,(format nil "Lee el estado del botón ~A con debounce" name)
       (let ((current-state (gpio-ffi:gpio-write ,pin nil)))
         (delay-microseconds ,debounce-time)
         (when (eq current-state (gpio-ffi:gpio-write ,pin nil))
           (iiscv:make-atomic-commit
            `(setf ,(intern (format nil "*~A-LAST-STATE*" name) ,current-state))
           (setf ,(intern (format nil "*~A-LAST-STATE*" name) current-state))
           current-state)))))

(defmacro def-relay (name pin &key (activation-delay 20))
  "Define un relé con seguridad de conmutación forzada.

   NAME: Nombre del relé
   PIN: Número de pin
   ACTIVATION-DELAY: Delay mínimo en milisegundos entre cambios"

  `(progn
     (def-pin ,name ,pin :mode :output :critical t)

     (defvar ,(intern (format nil "*~A-LAST-CHANGE*" name)) 0)

     (defun ,(intern (format nil "ACTIVATE-~A" name)) ()
       ,(format nil "Activa el relé ~A con verificación de delay de seguridad" name)
       (let ((current-time (get-universal-time)))
         (when (< (- current-time ,(intern (format nil "*~A-LAST-CHANGE*" name)))
                  (/ ,activation-delay 1000))
           (error "Relé ~A: Violación de delay de seguridad" name))
         (iiscv:make-atomic-commit
          `(defun ,(intern (format nil "ACTIVATE-~A" name)) ()
             ,(format nil "Activa el relé ~A" name)
             (gpio-ffi:gpio-write ,pin 1)))
         (setf ,(intern (format nil "*~A-LAST-CHANGE*" name) current-time)
               (gpio-ffi:gpio-write ,pin 1)
               (update-pin-statistics ',name 1)
               1)))

     (defun ,(intern (format nil "DEACTIVATE-~A" name)) ()
       ,(format nil "Desactiva el relé ~A con verificación de delay de seguridad" name)
       (let ((current-time (get-universal-time)))
         (when (< (- current-time ,(intern (format nil "*~A-LAST-CHANGE*" name)))
                  (/ ,activation-delay 20))
           (error "Relé ~A: Violación de delay de seguridad" name))
         (iiscv:make-atomic-commit
          `(defun ,(intern (format nil "DEACTIVATE-~A" name)) ()
             ,(format nil "Desactiva el relé ~A" name)
             (gpio-ffi:gpio-write ,pin 0)))
         (setf ,(intern (format nil "*~A-LAST-CHANGE*" name) current-time)
               (gpio-ffi:gpio-write ,pin 0)
               (update-pin-statistics ',name 0)
               0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros de Control de Flujo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gpio-safe (&body body)
  "Ejecuta body con manejo de errores y rollback seguro."

  `(handler-case
       (progn ,@body)
     (error (e)
       (format t "~%~%ERROR: Falla en operación GPIO: ~A" e)
       (format t "~%Iniciando rollback seguro...~%")
       (gpio-emergency-shutdown)
       (error "Operación GPIO abortada. Sistema en estado seguro"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones Auxiliares
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gpio-violations* nil
  "Lista de violaciones GPIO detectadas por el motor LISA.")

(defun gpio-violations ()
  "Retorna la lista de violaciones GPIO actuales."
  *gpio-violations*)

(defun lisa:assert (fact)
  "Sobrescribe assert para capturar violaciones GPIO."
  (lisa-lisp:assert fact)
  (lisa-lisp:run)
  (setf *gpio-violations*
        (loop for fact being each (lisa-lisp:facts)
              when (typep fact 'gpio-violation)
                collect fact)))

(defun update-pin-statistics (pin-name state)
  "Actualiza estadísticas de activación para auditoría de frecuencia."
  (let* ((pin-data (gethash pin-name *active-pins*))
           (freq (getf pin-data :activation-frequency))
           (new-freq (1+ freq)))
    (setf (gethash pin-name *active-pins*)
          (list :pin-name pin-name
                :last-activation (get-universal-time)
                :activation-frequency new-freq))))

(defun gpio-emergency-shutdown ()
  "Apaga todos los pines en orden inverso de inicialización."
  (format t "~%~%EMERGENCY SHUTDOWN: Apagando todos los pines...~%")
  (dolist (pin-spec (reverse *pin-initialization-order*))
    (let ((pin-name (car pin-spec))
          (pin-number (cadr pin-spec)))
      (format t "  Apagando pin ~A (~A)~%" pin-name pin-number)
      (gpio-ffi:gpio-write pin-number 0))))

(defun start-gpio-system ()
  "Inicializa el sistema GPIO completo."
  (format t "~%~%Iniciando sistema GPIO con IISCV...~%")
  (gpio-ffi:safe-gpio-init)
  (format t "~%Sistema GPIO listo.~%")
  (format t "~%Pines activos registrados:~%")
  (maphash (lambda (name data)
              (format t "  ~A: ~A~%" name data))
            *active-pins*))

(defun stop-gpio-system ()
  "Detiene el sistema GPIO de forma segura."
  (format t "~%~%Deteniendo sistema GPIO...~%")
  (gpio-emergency-shutdown)
  (format t "~%Sistema GPIO detenido.~%"))

(defun get-last-pin-configuration (pin-name)
  "Retorna la última configuración de un pin desde el historial IISCV."
  (iiscv:get-source-form (format nil "~A::~A"
                                          (package-name (symbol-package pin-name))
                                          (symbol-name pin-name))))
