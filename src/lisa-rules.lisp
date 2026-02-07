;;; lisa-rules.lisp
;;; Quality Audit & Forensic Logic Rules for IISCV

(in-package :iiscv)

(defvar *audit-violations* nil
  "A global list to collect messages from audit rules during a commit.")

(lisa-lisp:make-inference-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. FACT TEMPLATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; "Primary fact representing the static and logic analysis of a code definition."
;;;

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
  (slot contains-heavy-consing-loop-p)
  (slot style-critiques)
  (slot is-redefining-core-symbol-p)
  (slot calls)
  (slot status)              
  (slot has-side-effects-p)      
  (slot returns-constant-nil-p)  
  (slot has-dead-code-p)         
  (slot mutated-symbols)         
  (slot is-predicate-p)
  (slot is-recursive-p)
  (slot has-unbounded-loop-p)
  (slot assertion-count))

;;;
;;; "Control fact for managing multi-step reasoning (e.g., Impact Analysis, Logic Validation)."
;;;

(deftemplate goal ()
  (slot type)     
  (slot target)   
  (slot status))  

;;;
;;; "Fact generated when a quality or safety rule is triggered."
;;; (slot severity) ; :info, :warning, :error
;;;

(deftemplate violation ()
  (slot rule-id) 
  (slot severity)
  (slot message)
  (slot score)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. COLLECTOR / BRIDGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; "Intercepts violation facts and pushes them to the global Lisp list for the REPL."
;;;

(defrule rule-bridge-violations ()
  
  (?v (violation (rule-id ?id) (severity ?s) (message ?m) (score ?sc)))
  =>
  (push (list ?m ?s ?id ?sc) iiscv::*audit-violations*)
  (retract ?v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. QUALITY & MAINTAINABILITY RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; "Detects complex control flow using McCabe metric."
;;;

(defrule rule-1-1-high-cyclomatic-complexity ()
  (code-commit-analysis (symbol-name ?name) 
                        (cyclomatic-complexity ?cc))
  
  (test (and (numberp ?cc) (> ?cc 7)))
  
  =>
  (assert (violation (rule-id "1.1")
            (severity :error)
            (message (format nil "High cyclomatic complexity (~a) found in '~a'." ?cc ?name))
            (score 10))))


;;;
;;; "Detects oversized functions that hinder readability."
;;;

(defrule rule-1-2-function-too-long ()
  (code-commit-analysis (symbol-name ?name) (body-length ?len))
  (test (and (numberp ?len) (> ?len 25)))
  =>
  (assert (violation (rule-id "1.2")
            (severity :warning)
            (message (format nil "Function '~a' exceeds the 25-line limit (length: ~a)." ?name ?len))
	    (score 5))))

;;;
;;; "Detects un-named numeric literals."
;;;

;; (defrule rule-1-3-magic-number-usage ()
;;   (code-commit-analysis (symbol-name ?name) (magic-numbers ?nums))
;;   (test (not (null ?nums)))
;;   =>
;;   (assert (violation (rule-id "1.3")
;;             (severity :warning)
;;             (message (format nil "Magic numbers ~a found in '~a'. Define them as constants." ?nums ?name))
;; 	    (score 4))))


(defrule rule-1-3-magic-number-usage ()
  (code-commit-analysis (symbol-name ?name) (magic-numbers ?nums))
  (test (not (null ?nums)))
  =>
  (let* ((num-count (length (car ?nums)))
         (penalty (if (> num-count 3) 10 4))
         (msg (format nil "Se detectaron ~A números mágicos ~a en '~a'." 
                      num-count ?nums ?name)))
    (eval `(lisa:assert (violation (rule-id "1.3")
                                  (severity :warning)
                                  (message ,msg)
                          (score ,penalty))))))



(defrule rule-1-4-unused-parameters ()
  (code-commit-analysis (symbol-name ?name) (unused-parameters ?params))
  (test (not (null ?params)))
  =>
  (let* ((param-count (length ?params))
         ;; Restamos 3 puntos base + 2 por cada parámetro extra
         (penalty (+ 3 (* 2 (1- param-count))))
         (msg (format nil "Parámetros no usados detectados en '~a': ~a. Limpia la interfaz para mejorar el ELO." 
                      ?name ?params)))
    (eval `(lisa:assert (violation (rule-id "1.4")
                                  (severity :warning)
                                  (message ,msg)
                          (score ,penalty))))))


(defrule rule-1-5-heavy-consing-loop ()
  (code-commit-analysis (symbol-name ?name) (contains-heavy-consing-loop-p t))
  =>
  (assert (violation (rule-id "1.5")
            (severity :warning)
            (message (format nil "Performance Warning: Heavy consing detected inside loops in '~a'. Consider pre-allocation." ?name))
            (score 8))))



;;;
;;; "Ensures all definitions are documented."
;;;

(defrule rule-5-1-missing-docstring ()
  (code-commit-analysis (symbol-name ?name) (has-docstring-p nil))
  =>
  (assert (violation (rule-id "5.1")
            (severity :info)
            (message (format nil "Symbol '~a' is missing a docstring." ?name))
	    (score 1))))

;;;
;;; "Triggers style recommendations from the Lisp Critic."
;;;

(defrule rule-idiomatic-lisp-style ()
  (code-commit-analysis (symbol-name ?name) (style-critiques ?c))
  (test (not (null ?c)))
  =>
  (assert (violation (rule-id "IDIOMATIC-01")
            (severity :warning)
            (message (format nil "Style recommendations for '~a': ~a" ?name ?c))
	    (score 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. SECURITY & RELIABILITY RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; "Alerts when a function already exists in the forensic history."
;;;

(defrule rule-2-2-internal-redefinition ()
  (code-commit-analysis (symbol-name ?name) (is-redefining-core-symbol-p t))
  =>
  (assert (violation (rule-id "2.2")
            (severity :info) 
            (message (format nil "Mutation detected: '~a' has been updated in the history." ?name))
	    (score 2))))


;;;
;;; "Detects external OS command execution."
;;;

(defrule rule-3-1-unsafe-command-execution ()
  (code-commit-analysis (symbol-name ?name) (uses-unsafe-execution-p t))
  =>
  (assert (violation (rule-id "3.1")
            (severity :error)
            (message (format nil "Unsafe command execution in '~a'. Sanitize all inputs." ?name))
	    (score 20))))


(defrule rule-6-1-implementation-specific-symbols ()
  (code-commit-analysis (symbol-name ?name)
                        (uses-implementation-specific-symbols-p t))
  =>
  (assert (violation (rule-id "6.1")
                     (severity :warning)
                     (message (format nil "Use of non-portable, implementation-specific symbols detected in '~a'." ?name))
                     (score 8))))


(defrule rule-1-6-variable-mutation ()
  (code-commit-analysis (symbol-name ?name) (mutated-symbols ?syms))
  (test (not (null ?syms)))
  =>
  (let* ((mut-count (length ?syms))
         ;; 4 puntos base + 2 por cada variable mutada adicional
         (penalty (+ 4 (* 2 (1- mut-count))))
         (msg (format nil "Side-effect Warning: Variables mutadas en '~a': ~a. Considera un enfoque funcional." 
                      ?name ?syms)))
    (eval `(lisa:assert (violation (rule-id "1.6")
                                  (severity :warning)
                                  (message ,msg)
                          (score ,penalty))))))


(defrule rule-1-7-constant-nil-return ()
  (code-commit-analysis (symbol-name ?name) 
                        (returns-constant-nil-p t)
                        (is-predicate-p ?p))
  ;; Solo disparamos si ?p es explícitamente NIL. 
  ;; Si es T, la regla simplemente se ignora (que es lo que queremos para TEST-P).
  (test (eq ?p nil)) 
  =>
  (assert (violation (rule-id "1.7")
                     (severity :warning)
                     (message (format nil "Logic Warning: '~a' siempre retorna NIL. ¿Es una implementación incompleta?" ?name))
                     (score 5))))



;;;
;;; "Prevents stable (:curated) functions from depending on :experimental code."
;;;

(defrule rule-safety-curation-leak ()
  (code-commit-analysis (symbol-name ?caller) (status :curated) (calls ?calls))
  =>
  (dolist (callee ?calls)
    (let* ((v (iiscv::find-vertex-by-symbol-name callee))
           (data (when v (cl-graph:element v))))
      (when (and data (eq (getf data :status) :experimental))
        (assert (violation (rule-id "SAFETY-01")
                  (severity :error)
                  (message (format nil "Curation Leak: Stable function '~A' depends on experimental '~A'." ?caller callee))
		  (score 15)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. LOGIC & NASA JPL (POWER OF TEN) RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; "Automatically triggers a deep logic validation for every commit."
;;;

(defrule rule-trigger-logical-audit ()
  (code-commit-analysis (symbol-name ?name))
  =>
  (assert (goal (type validate-logic) (target ?name) (status active))))

;;;
;;; "Detects dead code branches (NASA JPL Rule 1)."
;;;


(defrule rule-logic-unreachable-code ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (has-dead-code-p t))
  =>
  (assert (violation (rule-id "LOGIC-02")
            (severity :error)
            (message (format nil "Dead Code in '~A': Unreachable branches detected." ?name))
	    (score 12))))

;;;
;;;  "Prohibits direct recursion in critical systems (NASA JPL Rule 1)."
;;;

(defrule rule-nasa-01-no-recursion ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (is-recursive-p t))
  =>
  (assert (violation (rule-id "NASA-01")
            (severity :warning)
            (message (format nil "Recursion Violation: '~A' calls itself. Prohibited in high-integrity code." ?name))
	    (score 7))))

;;;
;;;  "Ensures all loops have a defined exit condition (NASA JPL Rule 2)."
;;;

(defrule rule-nasa-02-unbounded-loop ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (has-unbounded-loop-p t))
  =>
  (assert (violation (rule-id "NASA-02")
            (severity :error)
            (message (format nil "Unbounded Loop in '~A': All loops must have an exit clause." ?name))
	    (score 15))))


;;;
;;; "Encourages defensive programming (NASA JPL Rule 5)."
;;;


(defrule rule-nasa-05-assertion-density ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (assertion-count ?count) (body-length ?len))
  (test (and (numberp ?len) (> ?len 10) (zerop ?count)))
  =>
  (assert (violation (rule-id "NASA-05")
            (severity :warning)
            (message (format nil "Low Assertion Density in '~A': No safety checks (assert/check-type) found." ?name))
	    (score 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. IMPACT ANALYSIS (BACKWARD CHAINING)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  "Detects redefinitions and triggers a forensic impact trace."
;;;

(defrule rule-trigger-impact ()
  (code-commit-analysis (symbol-name ?name) (is-redefining-core-symbol-p t))
  =>
  (assert (goal (type trace-impact) (target ?name) (status active))))

;;;
;;; "Navigates the CL-GRAPH to find all active dependents of the target."
;;; 

(defrule rule-process-impact ()
  (?g (goal (type trace-impact) (target ?target) (status active)))
  =>
  (let ((dependents (iiscv::find-dependents-in-history ?target)))
    (dolist (dep dependents)
      (assert (violation (rule-id "LOGIC-IMPACT")
                (severity :warning)
                (message (format nil "Forensic Impact: '~A' depends on '~A' and requires review." dep ?target))
		(score 4)))))
  (retract ?g))

