;;; ALL-RULES-TEST.lisp
;;; Pruebas individuales bottom-up para todas las reglas
;;; 
;;; USO: Evaluar una forma a la vez y observar las violaciones

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SECCIÓN 1: REGLAS ATÓMICAS (Probar una a una)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REGLA 1.1: Alta Complejidad Ciclomática (> 7)
;; Debe disparar: rule-1-1-high-cyclomatic-complexity
(make-atomic-commit 
  '(defun test-atomic-complexity (x y z)
     "Complejidad ciclomática ~15"
     (cond ((> x 10)
            (cond ((> y 20)
                   (cond ((> z 30) 'a)
                         ((< z 20) 'b)
                         ((= z 25) 'c)
                         (t 'd)))
                  ((< y 10) 'e)
                  (t 'f)))
           ((< x 5)
            (case y
              (1 'g)
              (2 'h)
              (3 'i)
              (otherwise 'j)))
           ((= x 7) 'k)
           ((= x 8) 'l)
           (t 'm))))

;; REGLA 1.2: Función Muy Larga (> 25 líneas)
;; Debe disparar: rule-1-2-function-too-long
(make-atomic-commit
  '(defun test-atomic-long (x)
     "Función de 30+ líneas"
     (let ((a 1) (b 2) (c 3) (d 4) (e 5))
       (print a) (print b) (print c) (print d) (print e)
       (let ((f 6) (g 7) (h 8) (i 9) (j 10))
         (print f) (print g) (print h) (print i) (print j)
         (let ((k 11) (l 12) (m 13) (n 14) (o 15))
           (print k) (print l) (print m) (print n) (print o)
           (let ((p 16) (q 17) (r 18) (s 19) (t 20))
             (print p) (print q) (print r) (print s) (print t)
             (let ((u 21) (v 22) (w 23) (x2 24) (y2 25))
               (print u) (print v) (print w) (print x2) (print y2)
               (+ x a b c d e f g h i j k l m n o p q r s t u v w x2 y2))))))))

;; REGLA 1.3: Números Mágicos
;; Debe disparar: rule-1-3-magic-number-usage
(make-atomic-commit
  '(defun test-atomic-magic (x)
     "Usa 42 y 99 sin nombrar"
     (if (> x 42)
         (* x 99)
         (+ x 42))))

;; REGLA 5.1: Falta Docstring
;; Debe disparar: rule-5-1-missing-docstring
(make-atomic-commit
  '(defun test-atomic-no-doc (x)
     (+ x 1)))

;; REGLA NASA-01: Recursión Directa
;; Debe disparar: rule-nasa-01-no-recursion
(make-atomic-commit
  '(defun test-atomic-recursive (n)
     "Factorial recursivo"
     (if (<= n 1)
         1
         (* n (test-atomic-recursive (- n 1))))))

;; REGLA NASA-02: Loop sin Bounds
;; Debe disparar: rule-nasa-02-unbounded-loop
(make-atomic-commit
  '(defun test-atomic-unbounded (lst)
     "Loop potencialmente infinito"
     (loop for x on lst
           collect (car x))))

;; REGLA NASA-05: Baja Densidad de Aserciones
;; Debe disparar: rule-nasa-05-assertion-density
(make-atomic-commit
  '(defun test-atomic-no-asserts (x y z w a b c)
     "Larga sin aserciones"
     (let ((result 0))
       (setf result (+ x y z))
       (setf result (* result w))
       (setf result (- result a))
       (setf result (+ result b))
       (setf result (/ result c))
       (setf result (* result 2))
       result)))

;; REGLA LOGIC-02: Código Muerto
;; Debe disparar: rule-logic-unreachable-code
(make-atomic-commit
  '(defun test-atomic-dead (x)
     "Tiene código inalcanzable"
     (if t
         x
         (progn
           (print "nunca")  ; dead code
           (* x 2)))))      ; dead code

;; REGLA IDIOMATIC-01: Estilo No Idiomático
;; Debe disparar: rule-idiomatic-lisp-style (vía lisp-critic)
(make-atomic-commit
  '(defun test-atomic-idiom (lst)
     "Uso de setq en lugar de let"
     (setq sum 0)
     (dolist (x lst)
       (setq sum (+ sum x)))
     sum))

;; REGLA 2.2: Redefinición
;; Debe disparar: rule-2-2-internal-redefinition (al hacer el segundo commit)
(make-atomic-commit
  '(defun test-atomic-redef (x)
     "Versión original"
     (+ x 1)))

(make-atomic-commit
  '(defun test-atomic-redef (x)
     "VERSIÓN REDEFINIDA - debe detectar"
     (+ x 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SECCIÓN 2: REGLAS GLOBALES (Requieren múltiples funciones)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GLOBAL-01: Dependencia Mutua (A <-> B)
;; Debe disparar: detect-mutual-dependency
;; Orden: Definir A, luego B, luego auditar
(make-atomic-commit
  '(defun test-global-cycle-a (x)
     "Llama a cycle-b"
     (if (> x 0)
         (test-global-cycle-b (- x 1))
         x)))

(make-atomic-commit
  '(defun test-global-cycle-b (y)
     "Llama a cycle-a"
     (if (> y 0)
         (test-global-cycle-a (- y 1))
         y)))

;; GLOBAL-02: Llamada Huérfana
;; Debe disparar: detect-orphan-call
(make-atomic-commit
  '(defun test-global-orphan (x)
     "Llama a función inexistente"
     (+ x (nonexistent-function-12345 x))))

;; GLOBAL-03: Aridad Alta
;; Debe disparar: detect-arity-mismatch (si tiene > 5 args)
(make-atomic-commit
  '(defun test-global-arity (a b c d e f g h)
     "8 argumentos"
     (+ a b c d e f g h)))

;; GLOBAL-04: Función Hoja
;; Debe disparar: detect-isolated-function
(make-atomic-commit
  '(defun test-global-leaf (x)
     "No llama a nada"
     (* x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SECCIÓN 3: FUNCIONES DE AYUDA PARA PRUEBAS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-atomic-all ()
  "Ejecuta todas las pruebas atómicas y crea un human commit"
  (format t "~%Creando human commit con todas las funciones de prueba...~%")
  (make-human-commit  "Test Suite - Reglas Atómicas" ))

(defun test-global-all ()
  "Crea human commit para probar reglas globales"
  (format t "~%Creando human commit para auditoría global...~%")
  (human-commit
    "Test Suite - Reglas Globales"
    '(test-global-cycle-a
      test-global-cycle-b
      test-global-orphan
      test-global-arity
      test-global-leaf)))

(defun test-run-global-audit ()
  "Ejecuta auditoría global sobre el último commit"
  (format t "~%Ejecutando auditoría global...~%")
  (audit-current-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUIA RÁPIDA DE USO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

MODO DE USO BOTTOM-UP (una regla a la vez):

1. Abrir REPL de iiscv:
   (iiscv:iiscv-repl)

2. Copiar y pegar UNA forma (make-atomic-commit ...) a la vez

3. Observar la salida - debería mostrar:
   [AUDIT] NOMBRE-FUNCION | Violations: N
   [REGLA-X] Mensaje de violación

4. Verificar que la regla correcta se disparó según los comentarios

5. Repetir para cada regla

EJEMPLO DE PRUEBA INDIVIDUAL:

IISCV-R> (make-atomic-commit 
           '(defun test-atomic-magic (x)
              "Usa 42 y 99 sin nombrar"
              (if (> x 42)
                  (* x 99)
                  (+ x 42))))

Salida esperada:
[AUDIT] TEST-ATOMIC-MAGIC | Violations: 1
[1.3] (4 pts) [WARNING] Magic numbers (42 99) found in 'TEST-ATOMIC-MAGIC'...
"9845-..."  ; UUID del commit

PRUEBA DE REGLAS GLOBALES:

IISCV-R> (test-atomic-all)  ; Crea commit con funciones atómicas
IISCV-R> (test-global-all)  ; Crea commit con funciones globales
IISCV-R> (test-run-global-audit)  ; Ejecuta auditoría global

Salida esperada de auditoría global:
========================================
   AUDITORÍA GLOBAL
   Hito: Test Suite - Reglas Globales
========================================
Cargando 5 definiciones...

----------------------------------------
RESULTADOS:
  Errores:   1
  Warnings:  1
  Info:      2
  Score:     21 puntos

Violaciones:
  [GLOBAL-02] (12 pts) [ERROR] Llamada huérfana: 'TEST-GLOBAL-ORPHAN' -> 'NONEXISTENT-FUNCTION-12345'
  [GLOBAL-01] ( 8 pts) [WARNING] Dependencia mutua: 'TEST-GLOBAL-CYCLE-A' <-> 'TEST-GLOBAL-CYCLE-B'
  [GLOBAL-03] ( 3 pts) [INFO] 'TEST-GLOBAL-ARITY' tiene 8 args obligatorios
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-LEAF'
========================================

|#

;;; Fin de ALL-RULES-TEST.lisp



;;;;;;;;;;;;;;;;;;; OUTPUT

IISCV> (make-atomic-commit 
  '(defun test-atomic-complexity (x y z)
     "Complejidad ciclomática ~15"
     (cond ((> x 10)
            (cond ((> y 20)
                   (cond ((> z 30) 'a)
                         ((< z 20) 'b)
                         ((= z 25) 'c)
                         (t 'd)))
                  ((< y 10) 'e)
                  (t 'f)))
           ((< x 5)
            (case y
              (1 'g)
              (2 'h)
              (3 'i)
              (otherwise 'j)))
           ((= x 7) 'k)
           ((= x 8) 'l)
           (t 'm))))


[AUDIT] TEST-ATOMIC-COMPLEXITY | Violations: 2 (1 errors, 1 warnings) | Total Score: 15
  [LOGIC-02] (12 pts) ERROR: Dead Code in 'TEST-ATOMIC-COMPLEXITY': Unreachable branches detected.
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-COMPLEXITY': Definition is somewhat too long! A "little" is probably OK,
"somewhat" might be OK, if this is a really complicated problem, but
code that is "too long" or "way too long" can almost certainly be
improved.
"773B7664-7314-4230-A655-8C08A1B21BF9"
IISCV> (make-atomic-commit
  '(defun test-atomic-long (x)
     "Función de 30+ líneas"
     (let ((a 1) (b 2) (c 3) (d 4) (e 5))
       (print a) (print b) (print c) (print d) (print e)
       (let ((f 6) (g 7) (h 8) (i 9) (j 10))
         (print f) (print g) (print h) (print i) (print j)
         (let ((k 11) (l 12) (m 13) (n 14) (o 15))
           (print k) (print l) (print m) (print n) (print o)
           (let ((p 16) (q 17) (r 18) (s 19) (t 20))
             (print p) (print q) (print r) (print s) (print t)
             (let ((u 21) (v 22) (w 23) (x2 24) (y2 25))
               (print u) (print v) (print w) (print x2) (print y2)
               (+ x a b c d e f g h i j k l m n o p q r s t u v w x2 y2))))))))

[AUDIT] TEST-ATOMIC-LONG | Violations: 2 (0 errors, 2 warnings) | Total Score: 7
  [1.3] ( 4 pts) WARNING: Magic numbers ((25 24 23 22 20 19 18 17 15 14 13 12 10 9 8 7 5 4 3 2)) found in 'TEST-ATOMIC-LONG'. Define them as constants.
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-LONG': Definition is way too long! A "little" is probably OK, "somewhat"
might be OK, if this is a really complicated problem, but code that
is "too long" or "way too long" can almost certainly be improved.

In general, FORMAT is used for most printing, because it's more
flexible.
"5A3188A6-9AFD-4E75-8054-ADB22353761A"
IISCV> (make-atomic-commit
  '(defun test-atomic-magic (x)
     "Usa 42 y 99 sin nombrar"
     (if (> x 42)
         (* x 99)
         (+ x 42))))

[AUDIT] TEST-ATOMIC-MAGIC | Violations: 1 (0 errors, 1 warnings) | Total Score: 4
  [1.3] ( 4 pts) WARNING: Magic numbers ((99 42)) found in 'TEST-ATOMIC-MAGIC'. Define them as constants.
"C678768E-B0D8-4909-8E9B-89A3BDAEAB17"
IISCV> (make-atomic-commit
  '(defun test-atomic-no-doc (x)
     (+ x 1)))

[AUDIT] TEST-ATOMIC-NO-DOC | Violations: 2 (0 errors, 1 warnings) | Total Score: 4
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-NO-DOC': Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.
  [5.1] ( 1 pts) INFO: Symbol 'TEST-ATOMIC-NO-DOC' is missing a docstring.
"9EA77881-11FE-4A41-89A9-E688558B41BA"
IISCV> (make-atomic-commit
  '(defun test-atomic-unbounded (lst)
     "Loop potencialmente infinito"
     (loop for x on lst
           collect (car x))))

[AUDIT] TEST-ATOMIC-UNBOUNDED | Violations: 0 (0 errors, 0 warnings) | Total Score: 0
"F3BEBF1A-9179-45BA-AC00-3B694CD92DFF"
IISCV> (make-atomic-commit
  '(defun test-atomic-no-asserts (x y z w a b c)
     "Larga sin aserciones"
     (let ((result 0))
       (setf result (+ x y z))
       (setf result (* result w))
       (setf result (- result a))
       (setf result (+ result b))
       (setf result (/ result c))
       (setf result (* result 2))
       result)))


[AUDIT] TEST-ATOMIC-NO-ASSERTS | Violations: 2 (0 errors, 2 warnings) | Total Score: 7
  [1.3] ( 4 pts) WARNING: Magic numbers ((2)) found in 'TEST-ATOMIC-NO-ASSERTS'. Define them as constants.
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-NO-ASSERTS': DECF would be simpler to subtract A from RESULT than SETF

INCF would be simpler to add B to RESULT than SETF
"B13EC7DB-99BB-4DE0-B8EE-CF40F33E5BFA"
IISCV> (make-atomic-commit
  '(defun test-atomic-dead (x)
     "Tiene código inalcanzable"
     (if t
         x
         (progn
           (print "nunca")  ; dead code
           (* x 2)))))    

[AUDIT] TEST-ATOMIC-DEAD | Violations: 3 (1 errors, 2 warnings) | Total Score: 19
  [LOGIC-02] (12 pts) ERROR: Dead Code in 'TEST-ATOMIC-DEAD': Unreachable branches detected.
  [1.3] ( 4 pts) WARNING: Magic numbers ((2)) found in 'TEST-ATOMIC-DEAD'. Define them as constants.
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-DEAD': Don't use IF and PROGN, use COND

In general, FORMAT is used for most printing, because it's more
flexible.
"05665F3B-CDDD-4583-8B17-DA78529445B3"
IISCV> (make-atomic-commit
  '(defun test-atomic-idiom (lst)
     "Uso de setq en lugar de let"
     (setq sum 0)
     (dolist (x lst)
       (setq sum (+ sum x)))
     sum))

[AUDIT] TEST-ATOMIC-IDIOM | Violations: 1 (0 errors, 1 warnings) | Total Score: 3
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-IDIOM': Don't use SETQ inside DOLIST to accumulate values for SUM.
Use DO. Make SUM a DO variable and don't use SETQ etc at all.

INCF would be simpler to add X to SUM than SETQ

GLOBALS!! Don't use global variables, i.e., SUM
"56C35ACE-EF94-4FA6-AD7B-4FFC7B87C3AA"
IISCV> (make-atomic-commit
  '(defun test-atomic-redef (x)
     "Versión original"
     (+ x 1)))


[AUDIT] TEST-ATOMIC-REDEF | Violations: 1 (0 errors, 1 warnings) | Total Score: 3
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-ATOMIC-REDEF': Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.
"BB533AC7-A53A-4738-9AB6-531234C91183"
IISCV> (make-atomic-commit
  '(defun test-atomic-redef (x)
     "VERSIÓN REDEFINIDA - debe detectar"
     (+ x 2)))

[AUDIT] TEST-ATOMIC-REDEF | Violations: 2 (0 errors, 1 warnings) | Total Score: 6
  [1.3] ( 4 pts) WARNING: Magic numbers ((2)) found in 'TEST-ATOMIC-REDEF'. Define them as constants.
  [2.2] ( 2 pts) INFO: Mutation detected: 'TEST-ATOMIC-REDEF' has been updated in the history.
"C02C7FAD-FECE-4148-84FB-A5D17155E075"
IISCV> (make-atomic-commit
  '(defun test-global-cycle-a (x)
     "Llama a cycle-b"
     (if (> x 0)
         (test-global-cycle-b (- x 1))
         x)))

[AUDIT] TEST-GLOBAL-CYCLE-A | Violations: 1 (0 errors, 1 warnings) | Total Score: 3
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-GLOBAL-CYCLE-A': Don't use (- X 1), use (1- X) for its value or (DECF X) to change X,
whichever is appropriate here.
"8BB8496E-4026-483F-AF57-3F96146FC672"
IISCV> (make-atomic-commit
  '(defun test-global-cycle-b (y)
     "Llama a cycle-a"
     (if (> y 0)
         (test-global-cycle-a (- y 1))
         y)))


[AUDIT] TEST-GLOBAL-CYCLE-B | Violations: 1 (0 errors, 1 warnings) | Total Score: 3
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'TEST-GLOBAL-CYCLE-B': Don't use (- Y 1), use (1- Y) for its value or (DECF Y) to change Y,
whichever is appropriate here.
"49C0F8F7-17C5-418F-ABAB-60C02B5E4C45"
IISCV> (make-atomic-commit
  '(defun test-global-orphan (x)
     "Llama a función inexistente"
     (+ x (nonexistent-function-12345 x))))


[AUDIT] TEST-GLOBAL-ORPHAN | Violations: 0 (0 errors, 0 warnings) | Total Score: 0
"645AF6BF-EBCD-425F-9F09-4DC0A104D556"
IISCV> (make-atomic-commit
  '(defun test-global-arity (a b c d e f g h)
     "8 argumentos"
     (+ a b c d e f g h)))


[AUDIT] TEST-GLOBAL-ARITY | Violations: 0 (0 errors, 0 warnings) | Total Score: 0
"3E0DD3D4-8081-4934-B396-5BFFB9100AA6"
IISCV> (make-atomic-commit
  '(defun test-global-leaf (x)
     "No llama a nada"
     (* x x)))

[AUDIT] TEST-GLOBAL-LEAF | Violations: 0 (0 errors, 0 warnings) | Total Score: 0
"14F44684-BB49-4EEC-ADFB-BA326687B192"
; processing (DEFUN TEST-ATOMIC-ALL ...)

; file: /tmp/slimevvTDiM
; in: DEFUN TEST-ATOMIC-ALL
;     (IISCV::MAKE-HUMAN-COMMIT)
; 
; caught STYLE-WARNING:
;   The function MAKE-HUMAN-COMMIT is called with zero arguments, but wants exactly one.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
; processing (DEFUN TEST-ATOMIC-ALL ...)
IISCV> (test-atomic-all)

Creando human commit con todas las funciones de prueba...
[CURATION] TEST-ATOMIC-COMPLEXITY promoted to :CURATED.
[CURATION] TEST-ATOMIC-LONG promoted to :CURATED.
[CURATION] TEST-ATOMIC-MAGIC promoted to :CURATED.
[CURATION] TEST-ATOMIC-NO-DOC promoted to :CURATED.
[CURATION] TEST-ATOMIC-UNBOUNDED promoted to :CURATED.
[CURATION] TEST-ATOMIC-NO-ASSERTS promoted to :CURATED.
[CURATION] TEST-ATOMIC-DEAD promoted to :CURATED.
[CURATION] TEST-ATOMIC-IDIOM promoted to :CURATED.
[CURATION] TEST-ATOMIC-REDEF promoted to :CURATED.
[CURATION] TEST-GLOBAL-CYCLE-A promoted to :CURATED.
[CURATION] TEST-GLOBAL-CYCLE-B promoted to :CURATED.
[CURATION] TEST-GLOBAL-ORPHAN promoted to :CURATED.
[CURATION] TEST-GLOBAL-ARITY promoted to :CURATED.
[CURATION] TEST-GLOBAL-LEAF promoted to :CURATED.

[IISCV] Human commit created: Test Suite - Reglas Atómicas
"9EAF85EC-621C-477E-B891-DBAC5042B7DA"
; processing (DEFUN TEST-RUN-GLOBAL-AUDIT ...)
IISCV> (test-run-global-audit)

Ejecutando auditoría global...

========================================
   AUDITORÍA GLOBAL
   Hito: Test Suite - Reglas Atómicas
========================================
Cargando 14 definiciones...

----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      1
  Score:     1 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      2
  Score:     2 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      3
  Score:     3 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      4
  Score:     4 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      5
  Score:     5 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      6
  Score:     6 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      7
  Score:     7 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      8
  Score:     8 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      9
  Score:     9 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   0
  Warnings:  0
  Info:      10
  Score:     10 puntos

Violaciones:
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   1
  Warnings:  0
  Info:      10
  Score:     22 puntos

Violaciones:
  [GLOBAL-02] (12 pts) [ERROR] Llamada huérfana: 'TEST-GLOBAL-CYCLE-B' -> 'IISCV::TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   1
  Warnings:  0
  Info:      11
  Score:     23 puntos

Violaciones:
  [GLOBAL-02] (12 pts) [ERROR] Llamada huérfana: 'TEST-GLOBAL-CYCLE-B' -> 'IISCV::TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-ORPHAN'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   1
  Warnings:  0
  Info:      12
  Score:     24 puntos

Violaciones:
  [GLOBAL-02] (12 pts) [ERROR] Llamada huérfana: 'TEST-GLOBAL-CYCLE-B' -> 'IISCV::TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-ARITY'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-ORPHAN'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================


----------------------------------------
RESULTADOS:
  Errores:   1
  Warnings:  0
  Info:      13
  Score:     25 puntos

Violaciones:
  [GLOBAL-02] (12 pts) [ERROR] Llamada huérfana: 'TEST-GLOBAL-CYCLE-B' -> 'IISCV::TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-LEAF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-ARITY'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-ORPHAN'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-GLOBAL-CYCLE-A'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-REDEF'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-IDIOM'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-DEAD'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-ASSERTS'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-UNBOUNDED'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-NO-DOC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-MAGIC'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-LONG'
  [GLOBAL-04] ( 1 pts) [INFO] Función hoja: 'TEST-ATOMIC-COMPLEXITY'
========================================

 
