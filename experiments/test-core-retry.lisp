;;; test-core-retry.lisp
;;; Retry Tests for Failed Rules

(in-package :iiscv)

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
                            sb-sys:*os-context*)))

;; Test 10: More Complex Returns with Magic Numbers
(make-atomic-commit '(defun complex-return-test (x y z)
                      (if (numberp x)
                          (values x (* x 2) (+ x 10) (- x 5) (/ x 3))
                          (values nil "Invalid" "Error"))))

(format t "~%~%=== RETRY TESTS COMPLETED ===~%")
(format t "Total retry tests: 10~%")
(format t "Run (show-atomic-commit) to view the commits~%")
(format t "Run (make-human-commit \"Retry test rules\") to consolidate~%")

;;; TEST-1 RESULT
;;; 
;;; 1. UNUSED PARAMETER (unused-param-test)
;;;    - Expected: Detect unused parameter 'unused1', 'unused2'
;;;    - Result: NO VIOLATIONS DETECTED
;;;    - Status: FAIL - Rule not implemented
;;; 
;;; 2. HEAVY CONSING LOOP (heavy-consing-test)
;;;    - Expected: Detect heavy consing in loop with collect
;;;    - Result: NO VIOLATIONS DETECTED
;;;    - Status: FAIL - Rule not implemented
;;; 
;;; 3. MAGIC NUMBERS - CONSTANTS (circle-area, temperature-conversion)
;;;    - Expected: Detect 3.14159, 9/5, 32 as magic numbers
;;;    - Result: 
;;;        circle-area: NO VIOLATIONS DETECTED (correct - constant is local)
;;;        temperature-conversion: MAGIC NUMBERS ((32 9/5)) DETECTED (incorrect - valid constants)
;;;    - Status: PARTIAL - Over-detects valid constants
;;; 
;;; 4. MAGIC NUMBERS - STATUS CODES (status-codes)
;;;    - Expected: Detect 200, 404, 500 as magic numbers
;;;    - Result: MAGIC NUMBERS ((500 404 200)) DETECTED (incorrect - valid status codes)
;;;    - Status: FAIL - Over-detects valid codes
;;; 
;;; 5. NASA RULES - CONTEXT (fibonacci-critical, bounded-loop-critical)
;;;    - Expected: Detect recursion and unbounded loops even in critical code
;;;    - Result: 
;;;        fibonacci-critical: RECURSION VIOLATION DETECTED (correct - recursion is recursion)
;;;        bounded-loop-critical: NO VIOLATIONS DETECTED (incorrect - loop has no exit clause)
;;;    - Status: PARTIAL - Inconsistent application
;;; 
;;; 6. HIGH COMPLEXITY (high-complexity-test)
;;;    - Expected: Detect high cyclomatic complexity (>7)
;;;    - Result: NO VIOLATIONS DETECTED
;;;    - Status: FAIL - Rule not triggered or threshold too high
;;; 
;;; 7. NESTED CONDITIONALS (nested-conditionals-test)
;;;    - Expected: Detect nested IFs and suggest COND
;;;    - Result: STYLE RECOMMENDATIONS DETECTED (correct - nested IFs detected)
;;;    - Status: PASS
;;; 
;;; 8. MULTIPLE SIDE EFFECTS (multiple-side-effects)
;;;    - Expected: Detect global variable mutation and parameter reassignment
;;;    - Result: STYLE RECOMMENDATIONS DETECTED (correct - globals detected)
;;;    - Status: PASS
;;; 
;;; 9. IMPLEMENTATION-SPECIFIC SYMBOLS (sbcl-features)
;;;    - Expected: Detect sb-ext, sb-c, sb-sys symbols
;;;    - Result: NO VIOLATIONS DETECTED
;;;    - Status: FAIL - Rule not implemented
;;; 
;;; 10. COMPLEX RETURNS WITH MAGIC NUMBERS (complex-return-test)
;;;     - Expected: Detect 2, 10, 5, 3 as magic numbers
;;;     - Result: MAGIC NUMBERS ((3 5 10 2)) DETECTED (incorrect - valid return values)
;;;     - Status: FAIL - Over-detects valid arithmetic

;;; SUMMARY:
;;; Correct detections: 4/10 (40%)
;;; Incorrect detections: 4/10 (40%)
;;; Not detected: 2/10 (20%)

;;; Key Issues Identified:
;;; 1. Magic number rule is too aggressive - detects valid constants and codes
;;; 2. NASA rules lack context awareness - treats all code as critical
;;; 3. Unused parameters rule not implemented
;;; 4. Heavy consing detection not implemented
;;; 5. Implementation-specific symbols rule not implemented

;;; Recommendations:
;;; - Add context parameter to NASA rules (critical vs non-critical)
;;; - Refine magic number detection with whitelist for common constants
;;; - Implement unused parameter detection
;;; - Implement heavy consing detection in loops
;;; - Implement implementation-specific symbols detection

;;; Overall: The audit engine needs refinement in rule specificity and context awareness.