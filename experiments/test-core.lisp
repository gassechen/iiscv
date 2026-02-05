;;; test-core.lisp
;;; Test Suite for IISCV Core Functions

(in-package :iiscv)

;; Test 1: Basic Function Definition
(make-atomic-commit '(defun sum (a b)
                      "Returns the sum of two numbers."
                      (+ a b)))

;; Test 2: Function with Complex Logic
(make-atomic-commit '(defun factorial (n)
                      "Calculates factorial recursively."
                      (if (= n 0)
                          1
                          (* n (factorial (- n 1))))))

;; Test 3: Function with Docstring Missing
(make-atomic-commit '(defun multiply (a b)
                      (* a b)))

;; Test 4: Function with Magic Numbers
(make-atomic-commit '(defun calculate-tax (amount)
                      (let ((tax-rate 0.21))
                        (* amount tax-rate))))

;; Test 5: Function with Unused Parameter
(make-atomic-commit '(defun process-data (data unused-param)
                      (format t "Processing data...~%")))

;; Test 6: Function with High Cyclomatic Complexity
(make-atomic-commit '(defun complex-logic (x y z)
                      (cond ((and (> x 10) (< y 5)) "Case 1")
                            ((or (= x 0) (= y 0)) "Case 2")
                            ((not (null z)) "Case 3")
                            (t "Default"))))

;; Test 7: Recursive Function (NASA Rule 1)
(make-atomic-commit '(defun fibonacci (n)
                      "Calculates Fibonacci number."
                      (if (<= n 1)
                          n
                          (+ (fibonacci (- n 1))
                             (fibonacci (- n 2))))))

;; Test 8: Function with Unbounded Loop (NASA Rule 2)
(make-atomic-commit '(defun infinite-loop (n)
                      (loop
                        (format t "Counting: ~A~%" n)
                        (incf n))))

;; Test 9: Function with Unsafe Execution
(make-atomic-commit '(defun run-command (cmd)
                      (uiop:run-program cmd :output :string)))

;; Test 10: Function with Implementation-Specific Symbols
(make-atomic-commit '(defun sb-specific (x)
                      (sb-ext:gc :full t)
                      x))

;; Test 11: Function with Heavy Consing Loop
(make-atomic-commit '(defun cons-heavy (n)
                      (loop for i from 1 to n
                            collect (list i i (* i i)))))

;; Test 12: Function with Assertions (NASA Rule 5)
(make-atomic-commit '(defun safe-divide (a b)
                      (assert (not (zerop b)) () "Division by zero!")
                      (/ a b)))

;; Test 13: Function with Dead Code
(make-atomic-commit '(defun dead-code-example (x)
                      (if t
                          (format t "Always true branch~%")
                          (format t "Never executed~%"))))

;; Test 14: Function with Side Effects
(make-atomic-commit '(defun has-side-effects (data)
                      (setf *global-var* data)
                      data))

;; Test 15: Function with Predicate Naming Convention
(make-atomic-commit '(defun is-positive-p (n)
                      (> n 0)))

;; Test 16: Function with Multiple Returns
(make-atomic-commit '(defun multiple-returns (x)
                      (if (evenp x)
                          (values x "even")
                          (values x "odd"))))

;; Test 17: Macro Definition
(make-atomic-commit '(defmacro my-when (condition &body body)
                      `(if ,condition
                           (progn ,@body))))

;; Test 18: Variable Definition
(make-atomic-commit '(defparameter *test-var* 42 "Test global variable."))

;; Test 19: Constant Definition
(make-atomic-commit '(defconstant +pi+ 3.14159 "Pi constant."))

;; Test 20: Type Definition
(make-atomic-commit '(defclass person ()
                      ((name :initarg :name :accessor name)
                       (age :initarg :age :accessor age))))

;; Test 21: Function with Multiple Parameters
(make-atomic-commit '(defun multi-param (a b c d e f g h i j)
                      (format t "Received: ~A~%" (list a b c d e f g h i j))))

;; Test 22: Function with Nested Conditionals
(make-atomic-commit '(defun nested-conditionals (x)
                      (if (> x 10)
                          (if (< x 20)
                              "Between 10 and 20"
                              "Greater than 20")
                          "Less than or equal to 10")))

;; Test 23: Function with Loop with Exit Condition
(make-atomic-commit '(defun bounded-loop (n)
                      (loop for i from 1 to n
                            do (format t "Iteration: ~A~%" i))))

;; Test 24: Function with Error Handling
(make-atomic-commit '(defun safe-operation (a b)
                      (handler-case
                          (/ a b)
                        (division-by-zero ()
                          (format t "Cannot divide by zero~%")
                          nil))))

;; Test 25: Function with Complex Return Values
(make-atomic-commit '(defun complex-return (x)
                      (if (numberp x)
                          (values x (* x 2) (+ x 10))
                          (values nil "Not a number"))))

;; Test 26: Function with Multiple Mutations
(make-atomic-commit '(defun multiple-mutations (list1 list2)
                      (push 1 list1)
                      (setf list2 (reverse list2))
                      (nconc list1 list2)))

;; Test 27: Function with Implementation-Specific Package
(make-atomic-commit '(defun cl-user-function ()
                      (cl-user::internal-function)))

;; Test 28: Function with Heavy Recursion
(make-atomic-commit '(defun deep-recursion (n depth)
                      (if (= depth 0)
                          n
                          (deep-recursion (+ n 1) (- depth 1)))))

;; Test 29: Function with Complex Data Structures
(make-atomic-commit '(defun complex-data (data)
                      (let ((hash (make-hash-table)))
                        (dolist (item data)
                          (setf (gethash (car item) hash) (cdr item)))
                        hash)))

;; Test 30: Function with Multiple Nested Loops
(make-atomic-commit '(defun nested-loops (n m)
                      (loop for i from 1 to n
                            do (loop for j from 1 to m
                                     do (format t "(~A,~A) " i j)))))

(format t "~%~%=== TEST CORE FUNCTIONS COMPLETED ===~%")
(format t "Total atomic commits created: 30~%")
(format t "Run (show-atomic-commit) to view the commits~%")
(format t "Run (make-human-commit \"Test core functions\") to consolidate~%")


;; Análisis Detallado de Corrección de las Reglas
;; ✅ TESTS CORRECTOS (Reglas bien aplicadas)
;; 1. Docstring Missing (Regla 5.1)
;; - CALCULATE-TAX, PROCESS-DATA, RUN-COMMAND, SB-SPECIFIC, CONS-HEAVY, SAFE-DIVIDE, HAS-SIDE-EFFECTS, IS-POSITIVE-P, MULTIPLE-RETURNS, PERSON, MULTI-PARAM, BOUNDED-LOOP, SAFE-OPERATION, COMPLEX-RETURN, MULTIPLE-MUTATIONS, CL-USER-FUNCTION, COMPLEX-DATA, NESTED-LOOPS
;; - Resultado: 18/18 correctos - Detecta perfectamente funciones sin docstring
;; 2. Dead Code (Regla LOGIC-02)
;; - COMPLEX-LOGIC - (IF T ...) tiene rama inalcanzable
;; - DEAD-CODE-EXAMPLE - (IF T ...) tiene rama inalcanzable
;; - Resultado: 2/2 correctos - Detecta perfectamente código muerto
;; 3. Unsafe Execution (Regla 3.1)
;; - RUN-COMMAND - Usa uiop:run-program
;; - Resultado: 1/1 correcto - Detecta perfectamente ejecución insegura
;; 4. Implementation-Specific Symbols (Regla implícita)
;; - SB-SPECIFIC - Usa sb-ext:gc
;; - Resultado: 1/1 correcto - Detecta símbolos específicos
;; 5. Style Recommendations (Regla IDIOMATIC-01)
;; - MY-WHEN - Usa IF sin ELSE + PROGN
;; - MULTIPLE-MUTATIONS - Reasigna parámetros
;; - DEEP-RECURSION - Usa (- N 1) en lugar de (1- N)
;; - Resultado: 3/3 correctos - Detecta perfectamente malas prácticas
;; ❌ TESTS INCORRECTOS (Reglas mal aplicadas o faltantes)
;; 1. Magic Numbers (Regla 1.3)
;; - FALLA: Detecta números en lugares incorrectos
;;   - FIBONACCI - Detecta (2) pero es correcto (caso base)
;;   - NESTED-CONDITIONALS - Detecta (10, 20) pero son límites válidos
;;   - COMPLEX-RETURN - Detecta (2, 10) pero son valores de retorno válidos
;; - Problema: Regla muy agresiva, detecta números en contextos legítimos
;; 2. Cyclomatic Complexity (Regla 1.1)
;; - FALLA: No detecta complejidad alta
;;   - COMPLEX-LOGIC tiene 4 decisiones + 1 = complejidad 5 (límite es 7)
;;   - Resultado: Correcto - No debería disparar
;; 3. Unused Parameters (Regla implícita)
;; - FALLA: No detecta PROCESS-DATA con unused-param
;; - Problema: Regla no implementada o no disparada
;; 4. NASA Rules (Reglas 01, 02, 05)
;; - FALLA: Detecta incorrectamente
;;   - FIBONACCI - Detecta recursión pero es función recursiva legítima
;;   - INFINITE-LOOP - Detecta loop no acotado pero es intencional
;;   - DEEP-RECURSION - Detecta recursión pero es función recursiva legítima
;; - Problema: Reglas NASA muy estrictas, no distinguen entre código crítico y no crítico
;; 5. Heavy Consing Loop (Regla implícita)
;; - FALLA: No detecta CONS-HEAVY con collect
;; - Problema: Regla no implementada o no disparada
;; 📊 RESUMEN FINAL
;; Correctos: 25/30 (83.3%)
;; Incorrectos: 5/30 (16.7%)
;; Problemas Principales:
;; 1. Regla de números mágicos muy agresiva
;; 2. Reglas NASA sin contexto de criticidad
;; 3. Reglas de parámetros sin usar no implementadas
;; 4. Detección de consing pesado no implementada
;; Recomendaciones:
;; - Ajustar regla de números mágicos para ignorar constantes matemáticas y casos base
;; - Implementar contexto para reglas NASA (solo en código crítico)
;; - Agregar detección de parámetros sin usar
;; - Implementar detección de consing pesado en loops

;; Test 1: Unused Parameter (Regla implícita que falló)
(make-atomic-commit '(defun unused-param-test (used unused1 unused2)
                      (format t "Used: ~A~%" used)
                      (* used 2)))
;; Test 2: Heavy Consing Loop (Regla implícita que falló)
(make-atomic-commit '(defun heavy-consing-test (n)
                      (loop for i from 1 to n
                            collect (list i (* i i) (/ i 2.0)))))
;; Test 3: Magic Numbers - Constants (Problema: Detecta constantes matemáticas)
(make-atomic-commit '(defun circle-area (radius)
                      (let ((pi 3.14159))
                        (* pi radius radius))))
(make-atomic-commit '(defun temperature-conversion (celsius)
                      (let ((fahrenheit (+ (* celsius 9/5) 32)))
                        fahrenheit)))
;; Test 4: Magic Numbers - Valid Return Values (Problema: Detecta valores de retorno)
(make-atomic-commit '(defun status-codes (code)
                      (case code
                        (200 "OK")
                        (404 "Not Found")
                        (500 "Server Error")
                        (t "Unknown"))))
;; Test 5: NASA Rules - Context (Problema: Sin contexto de criticidad)
(make-atomic-commit '(defun fibonacci-critical (n)
                      "Critical system Fibonacci calculation."
                      (if (<= n 1)
                          n
                          (+ (fibonacci-critical (- n 1))
                             (fibonacci-critical (- n 2))))))
(make-atomic-commit '(defun bounded-loop-critical (n max-iterations)
                      "Critical system bounded loop."
                      (loop for i from 1 to n
                            while (< i max-iterations)
                            do (format t "Iteration: ~A~%" i))))
;; Test 6: More Complex Logic for Cyclomatic Complexity
(make-atomic-commit '(defun high-complexity-test (x y z w)
                      (cond ((and (> x 10) (< y 5) (evenp z)) "Case 1")
                            ((or (= x 0) (= y 0) (= z 0)) "Case 2")
                            ((not (null w)) "Case 3")
                            ((and (numberp x) (stringp y)) "Case 4")
                            (t "Default"))))
;; Test 7: Multiple Nested Conditionals (Problema: Estilo anidado)
(make-atomic-commit '(defun nested-conditionals-test (a b c d)
                      (if (> a 10)
                          (if (< b 20)
                              (if (> c 5)
                                  "Deep 1"
                                  "Deep 2")
                              "Deep 3")
                          (if (evenp d)
                              "Deep 4"
                              "Deep 5"))))
;; Test 8: More Side Effects (Problema: Variables globales)
(make-atomic-commit '(defun multiple-side-effects (data1 data2)
                      (let ((temp (reverse data1)))
                        (setf *global-list* (append *global-list* temp))
                        (setf data2 (sort data2 #'<))
                        (values temp data2))))
;; Test 9: More Implementation-Specific Symbols
(make-atomic-commit '(defun sbcl-features ()
                      (list sb-ext:*gc-run-time*
                            sb-c::*compilation*
                            )))
;; Test 10: More Complex Returns with Magic Numbers
(make-atomic-commit '(defun complex-return-test (x y z)
                      (if (numberp x)
                          (values x (* x 2) (+ x 10) (- x 5) (/ x 3))
                          (values nil "Invalid" "Error"))))




;; Resultados TEST-1:
;; ❌ Problemas Identificados:
;; 1. Regla de Números Mágicos muy agresiva:
;;    - UNUSED-PARAM-TEST - Detecta 2 (número de retorno válido)
;;    - HEAVY-CONSING-TEST - Detecta 2.0 (constante matemática)
;;    - COMPLEX-RETURN-TEST - Detecta 2, 3, 5, 10 (operaciones aritméticas válidas)
;; 2. Reglas no implementadas:
;;    - UNUSED-PARAM-TEST - No detecta parámetros sin usar
;;    - HEAVY-CONSING-TEST - No detecta consing pesado
;;    - SBCL-FEATURES - No detecta símbolos específicos de implementación
;; 3. Reglas NASA sin contexto:
;;    - FIBONACCI-CRITICAL - Detecta recursión (correcto pero sin contexto)
;;    - BOUNDED-LOOP-CRITICAL - No detecta loop no acotado (incorrecto)
;; ✅ Detecciones Correctas:
;; - NESTED-CONDITIONALS-TEST - Detecta IFs anidados
;; - MULTIPLE-SIDE-EFFECTS - Detecta variables globales
;; - CIRCLE-AREA - No detecta 3.14159 (correcto - constante local)
;; Conclusión: El motor necesita refinamiento en especificidad y contexto.

;;TEST2

;; Test 1: Basic Magic Numbers (already tested but verify)
(make-atomic-commit '(defun basic-magic-test (x)
                      (let ((result (+ x 10)))
                        (* result 2))))
;; Test 2: Magic Numbers in Constants (local constants - should NOT detect)
(make-atomic-commit '(defun local-constants-test (radius)
                      (let ((pi 3.14159)
                            (tau (* 2 pi)))
                        (* pi radius radius))))
;; Test 3: Magic Numbers in Parameters (should NOT detect)
(make-atomic-commit '(defun parameters-test (base multiplier)
                      (let ((result (* base multiplier)))
                        (+ result 100))))
;; Test 4: Magic Numbers in Mathematical Constants (should NOT detect)
(make-atomic-commit '(defun math-constants-test (x)
                      (let ((e 2.71828)
                            (golden-ratio 1.61803)
                            (sqrt2 1.41421))
                        (+ x e golden-ratio sqrt2))))
;; Test 5: Magic Numbers in Common Constants (should NOT detect)
(make-atomic-commit '(defun common-constants-test (seconds)
                      (let ((minutes (* seconds 60))
                            (hours (* minutes 60))
                            (days (* hours 24)))
                        days)))
;; Test 6: Magic Numbers in Scientific Constants (should NOT detect)
(make-atomic-commit '(defun scientific-constants-test (mass)
                      (let ((g 9.81) ; gravity
                            (c 299792458) ; speed of light
                            (h 6.62607015e-34)) ; Planck constant
                        (* mass g))))
;; Test 7: Magic Numbers in Financial Constants (should NOT detect)
(make-atomic-commit '(defun financial-constants-test (amount)
                      (let ((tax-rate 0.21)
                            (conversion-rate 1.12))
                        (* amount tax-rate))))
;; Test 8: Magic Numbers in Status Codes (should NOT detect)
(make-atomic-commit '(defun status-codes-test (code)
                      (case code
                        (200 "OK")
                        (404 "Not Found")
                        (500 "Server Error")
                        (t "Unknown"))))
;; Test 9: Magic Numbers in Error Codes (should NOT detect)
(make-atomic-commit '(defun error-codes-test (error)
                      (case error
                        (1 "File not found")
                        (2 "Permission denied")
                        (3 "Out of memory")
                        (t "Unknown error"))))
;; Test 10: Magic Numbers in Configuration (should NOT detect)
(make-atomic-commit '(defun config-test (setting)
                      (case setting
                        (:debug t)
                        (:verbose 2)
                        (:quiet 0)
                        (t :normal))))
;; Test 11: Magic Numbers in Array Sizes (should NOT detect)
(make-atomic-commit '(defun array-test (n)
                      (let ((array (make-array 10)))
                        (dotimes (i n)
                          (setf (aref array i) i))
                        array)))
;; Test 12: Magic Numbers in Loop Bounds (should NOT detect)
(make-atomic-commit '(defun loop-test (n)
                      (loop for i from 1 to 100
                            do (format t "Iteration: ~A~%" i))
                      n))
;; Test 13: Magic Numbers in Mathematical Operations (should NOT detect)
(make-atomic-commit '(defun math-test (x)
                      (let ((result (+ (* x 2) 10)))
                        (if (> result 100)
                            (values result "High")
                            (values result "Low")))))
;; Test 14: Magic Numbers in Conditional Checks (should NOT detect)
(make-atomic-commit '(defun conditional-test (value)
                      (if (< value 10)
                          "Low"
                          (if (< value 100)
                              "Medium"
                              "High"))))
;; Test 15: Magic Numbers in String Operations (should NOT detect)
(make-atomic-commit '(defun string-test (text)
                      (let ((len (length text)))
                        (if (< len 5)
                            "Short"
                            (if (< len 20)
                                "Medium"
                                "Long")))))
;; Test 16: Magic Numbers in Time Operations (should NOT detect)
(make-atomic-commit '(defun time-test (timestamp)
                      (let ((now (get-universal-time))
                            (one-hour (* 60 60))
                            (one-day (* one-hour 24)))
                        (- now timestamp))))
;; Test 17: Magic Numbers in File Operations (should NOT detect)
(make-atomic-commit '(defun file-test (filename)
                      (with-open-file (stream filename
                                            :direction :output
                                            :if-exists :supersede
                                            :element-type 'character)
                        (format stream "File content"))))
;; Test 18: Magic Numbers in System Operations (should NOT detect)
(make-atomic-commit '(defun system-test ()
                      (let ((max-retries 3)
                            (timeout 30))
                        (loop for i from 1 to max-retries
                              do (format t "Attempt ~A~%" i)
                                 (sleep timeout))
                        t)))
;; Test 19: Magic Numbers in Data Processing (should NOT detect)
(make-atomic-commit '(defun data-test (data)
                      (let ((threshold 0.5)
                            (max-items 100))
                        (remove-if-not #'numberp data :count max-items :key #'abs :test #'> threshold))))
;; Test 20: Magic Numbers in Complex Calculations (should NOT detect)
(make-atomic-commit '(defun complex-test (x y z)
                      (let ((a (+ x y))
                            (b (* y z))
                            (c (- a b)))
                        (/ (+ a b c) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 1: Simple unused parameter
(make-atomic-commit '(defun simple-unused (used unused)
                      (format t "Using: ~A~%" used)
                      (* used 2)))
;; Test 2: Multiple unused parameters
(make-atomic-commit '(defun multi-unused (a b c d e)
                      (format t "Using only: ~A~%" a)
                      (* a 2)))
;; Test 3: Unused parameter in lambda
(make-atomic-commit '(defun lambda-unused (x)
                      (let ((f (lambda (y z) (+ x y))))
                        (funcall f 10 20))))
;; Test 4: Unused parameter with default value
(make-atomic-commit '(defun default-unused (required &optional unused default)
                      (format t "Required: ~A~%" required)
                      required))
;; Test 5: Unused parameter in nested function
(make-atomic-commit '(defun nested-unused (main unused1 unused2)
                      (let ((inner (lambda (unused3)
                                     (format t "Main: ~A~%" main))))
                        (funcall inner 100)
                        main)))
