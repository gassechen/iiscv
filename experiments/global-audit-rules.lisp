;;; global-audit-rules.lisp
;;; Reglas de Auditoría Global - Versión simplificada bottom-up
;;; Solo templates y reglas LISA, sin funciones auxiliares complejas

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMPLATES NUEVOS (Facts para análisis sistémico)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Representa una función/definición cargada en el sistema
(deftemplate systemic-definition ()
  (slot name)           ; Nombre completo (PACKAGE::FUNCTION)
  (slot arity)          ; Número de argumentos obligatorios
  (slot has-rest-p))    ; ¿Tiene &rest o &body?

;; Representa una llamada entre funciones
(deftemplate systemic-dependency ()
  (slot caller)         ; Quién llama
  (slot callee))        ; A quién llama

;; Meta de validación sistémica
(deftemplate systemic-goal ()
  (slot type)           ; :cross-reference, :cyclic-check, etc.
  (slot status))        ; :active, :completed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REGLAS DE AUDITORÍA GLOBAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REGLA GLOBAL-01: Detectar dependencias circulares simples (A llama a B, B llama a A)
;; Esto indica posible recursión mutua no intencional
(defrule detect-mutual-dependency ()
  "Detecta cuando dos funciones dependen mutuamente una de la otra"
  (systemic-goal (type :cross-reference) (status :active))
  (systemic-dependency (caller ?a) (callee ?b))
  (systemic-dependency (caller ?b) (callee ?a))
  (test (string< ?a ?b))  ; Evitar duplicados: solo reportar cuando a < b alfabéticamente
  =>
  (assert (violation 
            (rule-id "GLOBAL-01")
            (severity :warning)
            (message (format nil "Dependencia mutua detectada entre '~A' y '~A'. Revise si es intencional." ?a ?b))
            (score 8))))

;; REGLA GLOBAL-02: Detectar llamadas a funciones no definidas (huérfanas)
;; Excluye funciones del sistema Common Lisp
(defrule detect-orphan-call ()
  "Detecta llamadas a funciones que no están definidas en el grafo actual"
  (systemic-goal (type :cross-reference) (status :active))
  (systemic-dependency (caller ?caller) (callee ?callee))
  (not (systemic-definition (name ?callee)))
  =>
  (assert (violation 
            (rule-id "GLOBAL-02")
            (severity :error)
            (message (format nil "Llamada huérfana: '~A' llama a '~A' que no está definida en este commit" ?caller ?callee))
            (score 12))))

;; REGLA GLOBAL-03: Detectar inconsistencias de aridad (solo si sabemos la aridad exacta)
;; Nota: Esta regla se activa cuando tenemos información completa de aridad
(defrule detect-arity-mismatch ()
  "Detecta posibles errores de aridad en llamadas"
  (systemic-goal (type :cross-reference) (status :active))
  (systemic-dependency (caller ?caller) (callee ?callee))
  (systemic-definition (name ?callee) (arity ?expected) (has-rest-p nil))
  (test (> ?expected 5))  ; Solo flaggear funciones con muchos args (heurística simple)
  =>
  (assert (violation 
            (rule-id "GLOBAL-03")
            (severity :info)
            (message (format nil "Función '~A' tiene ~A argumentos obligatorios. Verifique llamadas desde '~A'." 
                           ?callee ?expected ?caller))
            (score 3))))

;; REGLA GLOBAL-04: Detectar funciones sin dependencias salientes (posibles hojas/islas)
;; Útil para identificar funciones que no utilizan utilidades comunes
(defrule detect-isolated-function ()
  "Detecta funciones que no llaman a ninguna otra función definida"
  (systemic-goal (type :cross-reference) (status :active))
  (systemic-definition (name ?f))
  (not (systemic-dependency (caller ?f)))
  =>
  (assert (violation 
            (rule-id "GLOBAL-04")
            (severity :info)
            (message (format nil "Función '~A' no tiene dependencias salientes (función hoja)." ?f))
            (score 1))))

;; REGLA GLOBAL-05: Detectar funciones muy acopladas (llaman a muchas otras)
;; Esto es un indicador de posible violación de Single Responsibility
(defrule detect-high-coupling ()
  "Función placeholder para detectar alto acoplamiento"
  (systemic-goal (type :cross-reference) (status :active))
  (systemic-definition (name ?f))
  =>
  ;; Nota: El conteo real de dependencias requeriría lógica adicional
  ;; Esta regla se activa para todas las funciones y el scoring se haría
  ;; basado en conteo post-ejecución
  (assert (violation 
            (rule-id "GLOBAL-05")
            (severity :info)
            (message (format nil "Función '~A' será analizada para acoplamiento." ?f))
            (score 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJEMPLO DE CÓMO SE USARÍA (documentación)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Para usar estas reglas:

1. Crear las dependencias desde el código:

   (lisa:assert (systemic-definition (name "MATH::ADD") (arity 2) (has-rest-p nil)))
   (lisa:assert (systemic-definition (name "MATH::SUB") (arity 2) (has-rest-p nil)))
   (lisa:assert (systemic-definition (name "MATH::CALC") (arity 2) (has-rest-p nil)))
   
   (lisa:assert (systemic-dependency (caller "MATH::CALC") (callee "MATH::ADD")))
   (lisa:assert (systemic-dependency (caller "MATH::CALC") (callee "MATH::SUB")))

2. Activar la auditoría:

   (lisa:assert (systemic-goal (type :cross-reference) (status :active)))

3. Ejecutar:

   (lisa:run)

4. Revisar violaciones en *audit-violations*

|#

;;; Fin de global-audit-rules.lisp