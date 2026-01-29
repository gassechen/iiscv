;;; lisa-rules.lisp

(in-package :iiscv)

(defvar *audit-violations* nil
  "A global list to collect messages from audit rules.")

(lisa-lisp:make-inference-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge Base
;;; Defines the templates and rules for static code analysis.
;;;;;;;;;;;;;;;;;;;;;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Fact Templates
;;;
;;;  "Template for facts representing the analysis of a single code commit."


(deftemplate code-commit-analysis ()
  (slot commit-uuid)          
  (slot symbol-name)          
  (slot symbol-type)          
  (slot body-length)          
  (slot has-docstring-p)      
  (slot magic-numbers)        
  (slot unused-parameters)    
  (slot cyclomatic-complexity)
  (slot uses-unsafe-execution-p) 
  (slot uses-implementation-specific-symbols-p) 
  (slot is-redefining-core-symbol-p) 
  (slot contains-heavy-consing-loop-p)
  (slot style-critiques)
  (slot logical-violations))


;;;   "Template for facts that represent a violation of a quality rule."

(deftemplate violation ()
  (slot rule-id) 
  (slot severity)
  (slot message)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule collect-violations ()
  "Collects all violation facts into a global list."
  (violation (message ?msg))
  =>
  (push (list ?msg)  *audit-violations*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quality Rules
;;;

;;;
;;; 1. Maintainability (Mantenibilidad)
;;;
;;;  "Fires when a function's cyclomatic complexity exceeds the threshold (e.g., 10)."
(defrule rule-1-1-high-cyclomatic-complexity ()
  (code-commit-analysis (symbol-name ?name)
                        (cyclomatic-complexity ?cc))
  (test (> ?cc 10))
  =>
  (assert (violation (rule-id "1.1")
            (severity :error)
            (message (format nil "High cyclomatic complexity (~a) found in '~a'. Consider refactoring." ?cc ?name)))))


;;; "Fires when a function's body length exceeds the threshold (e.g., 25)."
(defrule rule-1-2-function-too-long ()
  (code-commit-analysis (symbol-name ?name)
			(body-length ?len ))
  (test (> ?len 25))
  =>
  (assert (violation (rule-id "1.2")
            (severity :warning)
            (message (format nil "Symbol found in '~a' exceeds the 25-line limit (length: ~a)." ?name ?len)))))



;;;  "Fires when literal numbers (other than 0 or 1) are found in the code."
(defrule rule-1-3-magic-number-usage ()
  (code-commit-analysis (symbol-name ?name)
                        (magic-numbers ?nums))
  (test (not (null ?nums)))
  =>
  (assert (violation (rule-id "1.3")
                      (severity :warning)
                      (message (format nil "Magic numbers ~a found in '~a'. Define them as constants." ?nums ?name)))))

;;;
;;; 2. Reliability (Fiabilidad)
;;;

;;;  "Fires when a commit attempts to redefine a symbol from the COMMON-LISP package."
(defrule rule-2-2-core-symbol-redefinition ()
  (code-commit-analysis (symbol-name ?name)
			(is-redefining-core-symbol-p t))
  =>
  (assert (violation (rule-id "2.2")
	    (severity :error)
	    (message (format nil  "Attempt to redefine core symbol found in '~a'. This is highly dangerous." ?name)))))


;;;
;;; 3. Security (Seguridad)
;;;
;;; "Fires when a function appears to execute an external command."

(defrule rule-3-1-unsafe-command-execution ()
  (code-commit-analysis (symbol-name ?name)
                        (uses-unsafe-execution-p t))
  =>
  (assert (violation (rule-id "3.1")
                      (severity :error)
                      (message (format nil  "Unsafe command execution found in '~a'. Ensure all inputs are sanitized." ?name)))))


;;;
;;; 4. Performance Efficiency (Eficiencia de Desempeño)
;;;
;;; "Fires when memory allocation (consing) is detected inside a performance-critical loop."

(defrule rule-4-1-consing-in-loop ()
  (code-commit-analysis (symbol-name ?name)
                        (contains-heavy-consing-loop-p t))
  =>
  (assert (violation (rule-id "4.1")
                      (severity :warning)
                      (message (format nil  "Heavy consing detected in a loop found in '~a'. This may impact performance." ?name)))))


;;;
;;; 5. Functional Suitability (Idoneidad Funcional)
;;;
;;;"Fires when a function or class is defined without a docstring."
(defrule rule-5-1-missing-docstring ()
  (code-commit-analysis (symbol-name ?name)
                        (has-docstring-p nil))
  =>
  (assert (violation (rule-id "5.1")
                      (severity :info)
                      (message (format nil  "Symbol found in '~a' is missing a docstring." ?name)))))



;;;"Fires when a function has parameters that are declared but not used."
(defrule rule-5-2-unused-parameter ()
 (code-commit-analysis (symbol-name ?name)
		       (unused-parameters ?params))
  (test (not (null ?params)))
  =>
  (assert (violation (rule-id "5.2")
            (severity :warning)
            (message (format nil "Unused parameters ~a found in '~a'." ?params ?name)))))

;;;
;;; 6. Portability (Portabilidad)
;;;
;;;  "Fires when code uses symbols specific to a particular Common Lisp implementation."

(defrule rule-6-1-implementation-specific-symbols ()
  (code-commit-analysis (symbol-name ?name)
                        (uses-implementation-specific-symbols-p t))
  =>
  (assert (violation (rule-id "6.1")
                      (severity :warning)
            (message (format nil  "Use of non-portable, implementation-specific symbols found in '~a'." ?name)))))



;;;
;;; 10. Lisp critic
;;; "Fire lisp critic style"

(defrule rule-idiomatic-lisp-style ()
  (code-commit-analysis (symbol-name ?name)
                        (style-critiques ?c))
  (test (not (null ?c)))
  =>
  (assert (violation (rule-id "IDIOMATIC-01")
                     (severity :warning)
            (message (format nil "Style recommendations found in '~a':~%~a" ?name ?c)))))



;;;
;;; 11. Prolog
;;; Integrity (Integrity Axiom 1 - Hoare/Orphan References)
;;;
;;; "Fires when Prolog detects a reference to a function not defined in the image."

(defrule rule-11-1-logical-integrity-orphan ()
  (code-commit-analysis (symbol-name ?name)
                        (logical-violations ?c))
  (test (not (null ?c)))
  =>
  (dolist (err ?c)
    (assert (violation (rule-id "11.1")
                       (severity :error)
                       (message (format nil "Logic Integrity Violation in '~a': ~a" ?name err))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; TEST RULES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-all-rules ()
  "Runs a comprehensive test of all quality auditor rules."
  (lisa-lisp:reset)
  
  ;; Rule 1.1: High Cyclomatic Complexity
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-1-1")
                     (symbol-name "complex-function")
                     (body-length 0)
                     (cyclomatic-complexity 15)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 1.2: Function Too Long
  (lisa-lisp:assert (code-commit-analysis
                   (commit-uuid "test-rule-1-2")
                   (symbol-name "my-long-function")
                   (body-length 42)
                   (cyclomatic-complexity 0)
                   (magic-numbers nil)
                   (is-redefining-core-symbol-p nil)
                   (uses-unsafe-execution-p nil)
                   (contains-heavy-consing-loop-p nil)
                   (has-docstring-p t)
                   (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 1.3: Magic Number Usage
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-1-3")
                     (symbol-name "bad-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers '(5 100))
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 2.2: Core Symbol Redefinition
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-2-2")
                     (symbol-name "DEFUN")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p t)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 3.1: Unsafe Command Execution
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-3-1")
                     (symbol-name "exec-shell-command")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p t)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))

  ;; Rule 4.1: Consing in Loop
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-4-1")
                     (symbol-name "loop-with-cons")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p t)
                     (has-docstring-p t)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 5.1: Missing Docstring
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-5-1")
                     (symbol-name "undocumented-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                     (has-docstring-p nil)
                     (uses-implementation-specific-symbols-p nil)))
  
  ;; Rule 6.1: Implementation-specific Symbols
  (lisa-lisp:assert (code-commit-analysis
                     (commit-uuid "test-rule-6-1")
                     (symbol-name "sbcl-specific-function")
                     (body-length 0)
                     (cyclomatic-complexity 0)
                     (magic-numbers nil)
                     (has-docstring-p t)
                     (is-redefining-core-symbol-p nil)
                     (uses-unsafe-execution-p nil)
                     (contains-heavy-consing-loop-p nil)
                      (uses-implementation-specific-symbols-p t)))


  (lisa-lisp:assert (code-commit-analysis
                      (commit-uuid "test-rule-6-1")
                      (symbol-name "sbcl-specific-function")
                      (body-length 0)
                      (cyclomatic-complexity 0)
                      (magic-numbers nil)
                      (has-docstring-p t)
                      (is-redefining-core-symbol-p nil)
                      (uses-unsafe-execution-p nil)
                      (contains-heavy-consing-loop-p nil)
                      (uses-implementation-specific-symbols-p t)
		      (style-critiques "Don't use SETQ inside DOLIST. Use INCF instead.")))


  (lisa-lisp:assert (code-commit-analysis
                      (commit-uuid "test-rule-11-1")
                      (symbol-name "broken-logic-function")
                      (body-length 5)
                      (cyclomatic-complexity 1)
                      (magic-numbers nil)
                      (is-redefining-core-symbol-p nil)
                      (uses-unsafe-execution-p nil)
                      (contains-heavy-consing-loop-p nil)
                      (has-docstring-p t)
                      (unused-parameters nil)
                      (uses-implementation-specific-symbols-p nil)
                      (style-critiques nil)
                      ;; Inyectamos los errores que normalmente vendrían de run-prolog-integrity-audit
                      (logical-violations '("ORPHAN-REFERENCE: UNDEFINED-FUNC-X" 
                                            "ARITY-MISMATCH: + expected 2+, got 1"))))

  

  ;; Run the inference engine
  (lisa-lisp:run)
  
  ;; Display the final facts, which should include all the violations
  (lisa-lisp:facts))
