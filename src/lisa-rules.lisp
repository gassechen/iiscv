;;;lisa-rules.lisp
;;;Quality Audit & Forensic Logic Rules for IISCV

(in-package :iiscv)

(defvar *audit-violations* nil
  "A global list to collect messages from audit rules during a commit.")

(defvar *atomic-rete nil)

(setf *atomic-rete (lisa-lisp:make-inference-engine))

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
  (slot assertion-count)
  (slot definition-form))

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
  ;; Solo aplica a funciones/macros
  (code-commit-analysis (symbol-name ?name) (symbol-type function) (cyclomatic-complexity ?cc))
  (test (and (numberp ?cc) (> ?cc 7)))
  =>
  (assert (violation (rule-id "1.1")
            (severity :error)
            (message (format nil "High cyclomatic complexity (~a) found in function '~a'." ?cc ?name))
            (score 10))))


;;;
;;; "Detects oversized functions that hinder readability."
;;;
(defrule rule-1-2-function-too-long ()
  ;; Solo aplica a funciones
  (code-commit-analysis (symbol-name ?name) (symbol-type function) (body-length ?len))
  (test (and (numberp ?len) (> ?len 25)))
  =>
  (assert (violation (rule-id "1.2")
            (severity :warning)
            (message (format nil "Function '~a' exceeds the 25-line limit (length: ~a)." ?name ?len))
            (score 5))))


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
  ;; Solo tiene sentido en funciones que no son predicados explícitos
  (code-commit-analysis (symbol-name ?name) 
                        (symbol-type function)
                        (returns-constant-nil-p t)
                        (is-predicate-p nil))
  =>
  (assert (violation (rule-id "1.7")
                     (severity :warning)
                     (message (format nil "Logic Warning: '~a' siempre retorna NIL. ¿Es una implementación incompleta?" ?name))
                     (score 5))))

;;;
;;; "Ensures all definitions are documented."
;;;

(defrule rule-5-1-missing-docstring ()
  (code-commit-analysis (symbol-name ?name)
			(symbol-type function)
			(has-docstring-p nil))
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
  ;; Verificamos que no sea NIL y que, si es string, no esté vacío
  (test (and (not (null ?c))
             (or (not (stringp ?c))
                 (> (length (string-trim '(#\Space #\Newline #\Tab) ?c)) 0))))
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
  (code-commit-analysis (symbol-name ?name) 
                        (symbol-type ?type) 
                        (is-redefining-core-symbol-p t))
  =>
  (assert (violation (rule-id "2.2")
            (severity :info) 
            (message (format nil "Mutation: The ~A '~a' has been updated in the history." ?type ?name))
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
  (code-commit-analysis (symbol-name ?name) (symbol-type function))
  =>
  (assert (goal (type validate-logic) (target ?name) (status active))))

(defrule rule-logic-unreachable-code ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (symbol-type function) (has-dead-code-p t))
  =>
  (assert (violation (rule-id "LOGIC-02")
            (severity :error)
            (message (format nil "Dead Code in '~A': Unreachable branches detected." ?name))
            (score 12))))

(defrule rule-nasa-01-no-recursion ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) (symbol-type function) (is-recursive-p t))
  =>
  (assert (violation (rule-id "NASA-01")
            (severity :warning)
            (message (format nil "Recursion Violation: '~A' calls itself. Prohibited in high-integrity code." ?name))
            (score 7))))

(defrule rule-nasa-05-assertion-density ()
  (goal (type validate-logic) (target ?name) (status active))
  (code-commit-analysis (symbol-name ?name) 
                        (symbol-type function) ;; Indispensable para no molestar a las variables
                        (assertion-count ?count) 
                        (body-length ?len))
  (test (and (numberp ?len) (> ?len 10) (zerop ?count)))
  =>
  (assert (violation (rule-id "NASA-05")
            (severity :warning)
            (message (format nil "Low Assertion Density in '~A': No safety checks (assert/check-type) found." ?name))
            (score 6))))

;;; Regla para detectar captura genérica de errores sin acción (Silent Fail)
;;(defrule rule-nasa-10-silent-errors ()
;;  (goal (type validate-logic) (target ?name) (status active))
  ;; Necesitas que tu analizador previo ponga este hecho si detecta el patrón
;;  (code-commit-analysis (symbol-name ?name) (symbol-type function) (has-silent-error-p t))
;;  =>
;;  (assert (violation (rule-id "NASA-10")
;;           (severity :error)
;;            (message (format nil "Silent Fail in '~A': Generic error catching (NIL) hides bugs." ?name))
;;           (score 15))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule rule-fire-test-constant-integrity ()
  "Valida la integridad de constantes y variables globales."
  (goal (type audit) (target ?target) (status active))
  (code-commit-analysis (definition-form ?form) (symbol-name ?name))
  ;; Verificamos que sea un DEFCONSTANT o DEFPARAMETER
  (test (eq (get-commit-type ?form) 'variable))

  =>
  (format t "~%[ROVE-AUDIT] Validando constante/global: ~A..." ?name)
  (handler-case
      (progn
        (eval ?form) ;; La materializamos para que esté disponible
        (assert (goal (type validate-logic) (target ?target) (status approved)))
        (format t "~%[OK] Constante ~A activa en el sistema." ?name))
    (error (c)
      (format t "~%[ERROR] Fallo al definir constante: ~A" c)
      (assert (violation (rule-id "CONST-ERR") (score 100) (message "Error de definición")))
      (assert (goal (type validate-logic) (target ?target) (status failed))))))


;; dependencys 
(defrule rule-fire-test-dependency-integrity ()
  "Valida y carga dependencias usando el registro central de tipos."
  (goal (type audit) (target ?target) (status active))
  (code-commit-analysis (definition-form ?form) (symbol-name ?name))
  
  ;; Usamos tu función generalista
  (test (eq (get-commit-type ?form) 'dependency))
  =>
  (format t "~%[INFRA-AUDIT] Procesando Dependencia: ~A..." ?name)
  (handler-case
      (progn
        (eval ?form) 
        (assert (goal (type validate-logic) (target ?target) (status approved))))
    (error (c)
      (assert (violation (rule-id "DEP-ERR") (score 100) (message (format nil "~A" c))))
      (assert (goal (type validate-logic) (target ?target) (status failed))))))



(defrule rule-fire-test-rove-execution ()
  (goal (type audit) (target ?target) (status approved))
  (?g (goal (type validate-logic) (target ?target) (status active)))
  (code-commit-analysis (definition-form ?form) (symbol-name ?name))
  =>
  (format t "~%[ROVE-AUDIT] Verificando integridad de ~A..." ?target)
  
  ;; --- MEJORA: Scoping dinámico basado en el símbolo ---
  (let* ((target-symbol (find-symbol (string-upcase ?name)))
         ;; Si el símbolo no existe aún, usamos el paquete actual por defecto
         (target-package (if target-symbol 
                             (symbol-package target-symbol) 
                             *package*))
         (form-to-test ?form)
         (target-name ?name))
    
    (format t "~%[INFO] Compilando en contexto de paquete: ~A" (package-name target-package))
    
    (let ((*package* target-package)) ;; Anclaje dinámico
      (multiple-value-bind (compiled-fn warn-p fail-p)
          (compile nil `(lambda () ,form-to-test))
        (declare (ignore compiled-fn))
        
        (if (or warn-p fail-p)
            (progn
              (format t "~%[BLOQUEADO] Fallo de integridad en ~A: Símbolos indefinidos." target-name)
              (assert (violation (rule-id "COMPILATION-WARN")
                                 (severity :error)
                                 (message (format nil "Integrity check failed in package ~A" (package-name target-package)))
                                 (score 100)))
              (modify ?g (status failed)))
            (progn
              (format t "~%[OK] ~A validado correctamente." target-name)
              (modify ?g (status approved))))))))


(defrule rule-init-audit-goal ()
  (code-commit-analysis (symbol-name ?name))
  (not (goal (type audit) (target ?name)))
  =>
  (assert (goal (type audit) (target ?name) (status active))))

(defrule rule-evaluate-muro-threshold (:salience -100)
  ;; Aquí atrapamos la instancia del hecho en ?g 
  ;; PERO NO usamos slot-value luego. 
  (?g (goal (type audit) (target ?target) (status active)))
  =>
  (let ((total (calculate-total-score *audit-violations*)))
    ;; Usamos MODIFY de LISA, que es seguro.
    (if (<= total iiscv::*iiscv-tolerance*)
        (modify ?g (status approved))
        (modify ?g (status rejected)))
    
    ;; NO USES (slot-value ?g 'status) ACÁ. Es lo que hace explotar a SBCL.
    ;; Imprimimos un log genérico o usamos el valor que ya sabemos que tiene.
    (format t "~%[LISA-FORENSIC] Análisis de '~A' completado. Score: ~A" ?target total)))



(defrule rule-finalize-commit-to-graph ()
  ;; 1. Primero validamos que la meta esté aprobada (Backward Chaining result)
  (goal (type audit) (target ?target) (status approved))
  (goal (type validate-logic) (target ?target) (status approved))
  
  ;; 2. Buscamos el análisis que tiene el mismo nombre y extraemos el form
  (code-commit-analysis (symbol-name ?target) (definition-form ?form))
  =>
  ;; 3. Ahora sí, ?form existe en este contexto
  (format t "~%[MURO-OK] Paso de guardia exitoso. Registrando átomo '~A' en el grafo." ?target)
  (make-atomic-commit ?form))
