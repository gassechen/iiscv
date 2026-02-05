;;; experiment.lisp
;;; Propuesta MEJORADA de implementación de puntuación (scoring) para violaciones
;;; 
;;; Enfoque: Calcular el score DIRECTAMENTE en cada regla LISA, aprovechando
;;; el motor de inferencias. El slot 'score' se agrega al template violation.

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. CAMBIO EN EL TEMPLATE DE VIOLACIÓN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REEMPLAZAR en lisa-rules.lisp (líneas 58-61):

#|
(deftemplate violation ()
  (slot rule-id) 
  (slot severity)
  (slot message)
  (slot score))  ;; <-- NUEVO SLOT AGREGADO
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. TABLA DE PUNTUACIONES (referencia para las reglas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rule-scores*
  '(("3.1" . 20)        ;; Unsafe command execution - CRÍTICA
    ("NASA-02" . 15)    ;; Unbounded loop - Muy peligrosa
    ("SAFETY-01" . 15)  ;; Curation leak - Violación de arquitectura
    ("LOGIC-02" . 12)   ;; Dead code - Grave para mantenibilidad
    ("GLOBAL-01" . 12)  ;; Orphan call - Inconsistencia lógica
    ("1.1" . 10)        ;; High cyclomatic complexity
    ("NASA-01" . 7)     ;; Recursion in critical systems
    ("GLOBAL-02" . 8)   ;; Purity violation - Problema de diseño
    ("NASA-05" . 6)     ;; Low assertion density
    ("1.2" . 5)         ;; Function too long
    ("1.3" . 4)         ;; Magic numbers
    ("LOGIC-IMPACT" . 4);; Impact analysis
    ("2.2" . 2)         ;; Internal redefinition
    ("5.1" . 1)         ;; Missing docstring
    ("IDIOMATIC-01" . 3));; Style recommendations
  "Puntuación por ID de regla (cuanto más alto, más grave).")

(defun get-rule-score (rule-id)
  "Obtiene la puntuación para una regla específica."
  (or (cdr (assoc rule-id *rule-scores* :test #'equal))
      5)) ;; Valor por defecto si no está definida

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. REGLAS MODIFICADAS CON SCORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cada regla ahora incluye el score directamente en el assertion de violation

#|

;;; Regla 1.1: Complejidad ciclomática alta
(defrule rule-1-1-high-cyclomatic-complexity ()
  (code-commit-analysis (symbol-name ?name) (cyclomatic-complexity ?cc))
  (test (and (numberp ?cc) (> ?cc 7)))
  =>
  (assert (violation 
            (rule-id "1.1")
            (severity :error)
            (message (format nil "High cyclomatic complexity (~a) found in '~a'." ?cc ?name))
            (score 10))))  ;; <-- SCORE AGREGADO DIRECTAMENTE

;;; Regla 1.2: Función muy larga
(defrule rule-1-2-function-too-long ()
  (code-commit-analysis (symbol-name ?name) (body-length ?len))
  (test (and (numberp ?len) (> ?len 25)))
  =>
  (assert (violation 
            (rule-id "1.2")
            (severity :warning)
            (message (format nil "Function '~a' exceeds the 25-line limit (length: ~a)." ?name ?len))
            (score 5))))  ;; <-- SCORE AGREGADO

;;; Regla 1.3: Números mágicos
(defrule rule-1-3-magic-number-usage ()
  (code-commit-analysis (symbol-name ?name) (magic-numbers ?nums))
  (test (not (null ?nums)))
  =>
  (assert (violation 
            (rule-id "1.3")
            (severity :warning)
            (message (format nil "Magic numbers ~a found in '~a'. Define them as constants." ?nums ?name))
            (score 4))))  ;; <-- SCORE AGREGADO

;;; Regla 2.2: Redefinición interna
(defrule rule-2-2-internal-redefinition ()
  (code-commit-analysis (symbol-name ?name) (is-redefining-core-symbol-p t))
  =>
  (assert (violation 
            (rule-id "2.2")
            (severity :info)
            (message (format nil "Mutation detected: '~a' has been updated in the history." ?name))
            (score 2))))  ;; <-- SCORE AGREGADO

;;; Regla 3.1: Ejecución insegura de comandos (CRÍTICA)
(defrule rule-3-1-unsafe-command-execution ()
  (code-commit-analysis (symbol-name ?name) (uses-unsafe-execution-p t))
  =>
  (assert (violation 
            (rule-id "3.1")
            (severity :error)
            (message (format nil "Unsafe command execution in '~a'. Sanitize all inputs." ?name))
            (score 20))))  ;; <-- SCORE MUY ALTO

;;; Regla 5.1: Falta docstring
(defrule rule-5-1-missing-docstring ()
  (code-commit-analysis (symbol-name ?name) (has-docstring-p nil))
  =>
  (assert (violation 
            (rule-id "5.1")
            (severity :info)
            (message (format nil "Symbol '~a' is missing a docstring." ?name))
            (score 1))))  ;; <-- SCORE BAJO

;;; Regla IDIOMATIC-01: Estilo
(defrule rule-idiomatic-lisp-style ()
  (code-commit-analysis (symbol-name ?name) (style-critiques ?c))
  (test (not (null ?c)))
  =>
  (assert (violation 
            (rule-id "IDIOMATIC-01")
            (severity :warning)
            (message (format nil "Style recommendations for '~a': ~a" ?name ?c))
            (score 3))))  ;; <-- SCORE AGREGADO

;;; Regla SAFETY-01: Curation leak
(defrule rule-safety-curation-leak ()
  (code-commit-analysis (symbol-name ?caller) (status :curated) (calls ?calls))
  =>
  (dolist (callee ?calls)
    (let* ((v (iiscv::find-vertex-by-symbol-name callee))
           (data (when v (cl-graph:element v))))
      (when (and data (eq (getf data :status) :experimental))
        (assert (violation 
                  (rule-id "SAFETY-01")
                  (severity :error)
                  (message (format nil "Curation Leak: Stable function '~A' depends on experimental '~A'." ?caller callee))
                  (score 15)))))))  ;; <-- SCORE AGREGADO

;;; Regla LOGIC-02: Código muerto
(defrule rule-logic-unreachable-code ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (has-dead-code-p t))
  =>
  (assert (violation 
            (rule-id "LOGIC-02")
            (severity :error)
            (message (format nil "Dead Code in '~A': Unreachable branches detected." ?name))
            (score 12))))  ;; <-- SCORE AGREGADO

;;; Regla NASA-01: Recursión
(defrule rule-nasa-01-no-recursion ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (is-recursive-p t))
  =>
  (assert (violation 
            (rule-id "NASA-01")
            (severity :warning)
            (message (format nil "Recursion Violation: '~A' calls itself. Prohibited in high-integrity code." ?name))
            (score 7))))  ;; <-- SCORE AGREGADO

;;; Regla NASA-02: Loop sin bounds
(defrule rule-nasa-02-unbounded-loop ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (has-unbounded-loop-p t))
  =>
  (assert (violation 
            (rule-id "NASA-02")
            (severity :error)
            (message (format nil "Unbounded Loop in '~A': All loops must have an exit clause." ?name))
            (score 15))))  ;; <-- SCORE AGREGADO

;;; Regla NASA-05: Densidad de aserciones
(defrule rule-nasa-05-assertion-density ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (assertion-count ?count) (body-length ?len))
  (test (and (numberp ?len) (> ?len 10) (zerop ?count)))
  =>
  (assert (violation 
            (rule-id "NASA-05")
            (severity :warning)
            (message (format nil "Low Assertion Density in '~A': No safety checks (assert/check-type) found." ?name))
            (score 6))))  ;; <-- SCORE AGREGADO

;;; Regla LOGIC-IMPACT: Análisis de impacto
(defrule rule-process-impact ()
  (?g (goal (type trace-impact) (target ?target) (status active)))
  =>
  (let ((dependents (iiscv::find-dependents-in-history ?target)))
    (dolist (dep dependents)
      (assert (violation 
                (rule-id "LOGIC-IMPACT")
                (severity :warning)
                (message (format nil "Forensic Impact: '~A' depends on '~A' and requires review." dep ?target))
                (score 4)))))  ;; <-- SCORE AGREGADO
  (retract ?g))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. ACTUALIZACIÓN DEL RULE-BRIDGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REEMPLAZAR en lisa-rules.lisp (líneas 71-76):

#|
(defrule rule-bridge-violations ()
  (?v (violation (rule-id ?id) (severity ?s) (message ?m) (score ?score)))  ;; <-- AGREGAR SCORE
  =>
  (push (list ?m ?s ?id ?score) iiscv::*audit-violations*)  ;; <-- INCLUIR SCORE
  (retract ?v))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. FUNCIONES AUXILIARES PARA MANEJAR VIOLACIONES CON SCORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Estas funciones trabajan con el nuevo formato: (message severity rule-id score)

(defun violation-score (violation)
  "Extrae la puntuación de una violación (4to elemento)."
  (fourth violation))

(defun calculate-total-score (violations)
  "Suma todas las puntuaciones de las violaciones."
  (reduce #'+ (mapcar #'violation-score violations) :initial-value 0))

(defun get-severity-from-violation (violation)
  "Extrae la severidad de una violación."
  (second violation))

(defun filter-violations-by-severity (violations severity)
  "Filtra violaciones por nivel de severidad."
  (remove-if-not (lambda (v) (eq (get-severity-from-violation v) severity))
                 violations))

(defun sort-violations-by-score (violations)
  "Ordena violaciones de mayor a menor puntuación."
  (sort (copy-list violations) #'> :key #'violation-score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. REPORTE MEJORADO PARA MAIN.LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REEMPLAZAR en main.lisp (líneas 132-134) el formato de salida:

#|
;; CÓDIGO VIEJO:
(format t "~%[AUDIT] ~A | Violations: ~A~%" name (length *audit-violations*))
(format t "~{~a~%~}" (mapcar #'car *audit-violations*))
commit-uuid

;; CÓDIGO NUEVO:
(let* ((sorted-violations (sort-violations-by-score *audit-violations*))
       (total-score (calculate-total-score *audit-violations*))
       (error-count (length (filter-violations-by-severity *audit-violations* :error)))
       (warning-count (length (filter-violations-by-severity *audit-violations* :warning))))
  
  (format t "~%[AUDIT] ~A | Violations: ~A (~A errors, ~A warnings) | Total Score: ~A~%"
          name (length *audit-violations*) error-count warning-count total-score)
  
  ;; Imprimir cada violación con su score
  (dolist (v sorted-violations)
    (format t "  [~A] (~2D pts) ~A: ~A~%"
            (third v)           ;; rule-id
            (violation-score v) ;; score
            (second v)          ;; severity
            (first v)))         ;; message
  
  commit-uuid)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7. ACTUALIZACIÓN DE global-audit.lisp (si tiene reglas custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejemplo de cómo se verían las reglas en global-audit.lisp:

#|

(defrule rule-global-orphan-call ()
  (goal (type :global-validation) (status :active))
  (code-commit-analysis (symbol-name ?caller) (calls ?calls))
  =>
  (dolist (callee ?calls)
    (unless (lisa:find-fact-instance 'code-commit-analysis (symbol-name callee))
      (assert (violation 
                (rule-id "GLOBAL-01")
                (severity :error)
                (message (format nil "Inconsistencia de Hito: '~A' llama a '~A', pero '~A' no forma parte de este commit curado." 
                               ?caller callee callee))
                (score 12))))))  ;; <-- SCORE AGREGADO

(defrule rule-global-purity-violation ()
  (code-commit-analysis (symbol-name ?pure-fn) (has-side-effects-p nil) (calls ?calls))
  (code-commit-analysis (symbol-name ?dirty-fn) (has-side-effects-p t))
  (test (member ?dirty-fn ?calls :test #'equal))
  =>
  (assert (violation 
            (rule-id "GLOBAL-02")
            (severity :warning)
            (message (format nil "Violación de Pureza: '~A' (declarada pura) depende de '~A' (que tiene efectos de sitio)." 
                           ?pure-fn ?dirty-fn))
            (score 8))))  ;; <-- SCORE AGREGADO

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8. RESUMEN DE ARCHIVOS A MODIFICAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

1. src/lisa-rules.lisp
   - Agregar slot (score) al template 'violation' (línea 58-61)
   - Modificar rule-bridge-violations para incluir score (línea 71-76)
   - Agregar slot score a TODOS los assert de violation en las reglas

2. src/main.lisp
   - Modificar el formato de salida en make-atomic-commit (línea 132-134)
   - Agregar import de violation-score, calculate-total-score, etc. si es necesario

3. src/global-audit.lisp
   - Agregar slot score a los assert de violation en reglas custom

4. src/experiment.lisp (este archivo)
   - Mantener las funciones auxiliares: violation-score, calculate-total-score, etc.
   - Mantener *rule-scores* como referencia/documentación

VENTAJAS DE ESTE ENFOQUE:
- El score se calcula UNA SOLA VEZ en cada regla (eficiente)
- Aprovecha el motor LISA (no lógica externa)
- Fácil de ajustar: solo cambiar el número en la regla
- Escalable: nuevas reglas definen su propio score
- El rule-bridge solo transfiere datos, no calcula nada

|#

;;; Ejemplo de salida esperada después de los cambios:
#|

[AUDIT] MY-FUNCTION | Violations: 5 (2 errors, 2 warnings) | Total Score: 41
  [3.1] (20 pts) ERROR: Unsafe command execution in 'MY-FUNCTION'. Sanitize all inputs.
  [NASA-02] (15 pts) ERROR: Unbounded Loop in 'MY-FUNCTION'. All loops must have an exit clause.
  [1.1] (10 pts) ERROR: High cyclomatic complexity (12) found in 'MY-FUNCTION'.
  [1.2] ( 5 pts) WARNING: Function 'MY-FUNCTION' exceeds the 25-line limit (length: 30).
  [5.1] ( 1 pts) INFO: Symbol 'MY-FUNCTION' is missing a docstring.

|#

;;; Fin de experiment.lisp