;;lisp-prolog-rules.lisp
(in-package :iiscv)


(defun is-orphan (args bindings other-goals)
  (let ((sym (prolog:subst-bindings bindings (car args))))
    ;; Un símbolo es un huérfano SOLO SI:
    ;; NO está definido en Lisp (fboundp) 
    ;; Y ADEMÁS NO está en el grafo de IISCV (get-last-uuid-by-name)
    (if (and (not (fboundp sym))
             (not (get-last-uuid-by-name sym)))
        (prolog:prove-all other-goals bindings)
        prolog:fail)))


(defun prolog-not-equal (args bindings other-goals)
  (let ((x (prolog:subst-bindings bindings (car args)))
        (y (prolog:subst-bindings bindings (cadr args))))
    (if (not (equal x y))
        (prolog:prove-all other-goals bindings)
        prolog:fail)))


(defun not-a-valid-ref (args bindings other-goals)
  (let ((sym (prolog:subst-bindings bindings (car args))))
    ;; Si no es primitiva Y no tiene UUID en el mapa (o sea, no fue auditada)
    (if (and (not (prolog:prove-all `((prolog:is-primitive ,sym)) bindings))
             (not (get-last-uuid-by-name sym))) ;; <--- Directo con el símbolo
        (prolog:prove-all other-goals bindings)
        prolog:fail)))


(defun walk-and-assert-facts (name form)
  (let ((special-forms '(let let* lambda block progn if when cond setq setf quote))
        ;; Si es un DEFUN, saltamos 'defun', el nombre y la lambda-list.
        (body (if (eq (car form) 'defun) (cdddr form) (list form))))
    (labels ((walk (x)
               (cond 
                 ;; Si es un átomo (símbolo suelto o número), no hacemos nada.
                 ((atom x) nil)
                 
                 ;; Caso LET / LET*: Saltamos las variables y caminamos el cuerpo.
                 ((and (listp x) (member (car x) '(let let*)))
                  (dolist (item (cddr x)) (walk item)))

                 ;; Caso LLAMADA A FUNCIÓN: 
                 ((listp x)
                  (let ((func (car x))
                        (args (cdr x)))
                    ;; A. Si el primer elemento es un símbolo y no es "special", es una función.
                    (when (and (symbolp func) (not (member func special-forms)))
                      (prolog:add-clause `((prolog:depends-on ,name ,func)))
                      (prolog:add-clause `((prolog:actual-call-arity ,name ,func ,(length args)))))
                    
                    ;; B. Caminamos los argumentos uno por uno. 
                    ;; Si un argumento es otra lista (ej: (car (cdr x))), walk entrará ahí.
                    (dolist (arg args) (walk arg)))))))
      
      ;; Empezamos a caminar el cuerpo del DEFUN
      (dolist (line body) (walk line)))))


;; 3. Una versión segura de introspección para que no tire UNDEFINED-FUNCTION
(defun get-expected-arity-safe (sym)
  (when (fboundp sym)
    #+sbcl (let ((arglist (sb-introspect:function-lambda-list sym)))
             (if (listp arglist) (length arglist) nil))
    #-sbcl nil))


;; --- 1. CONFIGURACIÓN DEL MOTOR ---
(prolog:clear-db)

;; --- 2. AXIOMAS (LÓGICA) ---

;; Axioma 1: Referencias Huérfanas
(prolog:<- (prolog:inconsistency ?f prolog:orphan-reference ?dep)
           (prolog:depends-on ?f ?dep)
           (prolog:is-orphan ?dep))

;; Axioma 2: Error de Aridad
(prolog:<- (prolog:inconsistency ?f prolog:arity-mismatch ?dep)
           (prolog:depends-on ?f ?dep)
           (prolog:expected-arity ?dep ?expected)
           (prolog:actual-call-arity ?f ?dep ?actual)
           (prolog:not-equal ?expected ?actual))

;; Axioma de Integridad Referencial:
;; Usamos una keyword :referential-integrity para el tipo de error
(prolog:<- (prolog:inconsistency ?f :referential-integrity ?dep)
           (prolog:depends-on ?f ?dep)
           ;; Usamos el nombre del predicado sin el prefijo prolog:
           (not-a-valid-ref ?dep))




;; --- 3. CONECTORES LISP (PRIMITIVAS) ---

;; Registramos que estas funciones de Lisp actúan como predicados de Prolog


(setf (get 'prolog:is-orphan 'prolog:clauses) 'is-orphan)
(setf (get 'prolog:not-equal 'prolog:clauses) 'prolog-not-equal)
(setf (get 'prolog:collect-logic-violation 'prolog:clauses) 'collect-logic-violation)
(setf (get 'not-a-valid-ref 'prolog:clauses) 'not-a-valid-ref)

;; --- 4. HECHOS ESTÁTICOS ---
(prolog:<- (prolog:is-primitive +))
(prolog:<- (prolog:is-primitive -))
(prolog:<- (prolog:is-primitive *))
(prolog:<- (prolog:is-primitive /))
(prolog:<- (prolog:is-primitive list))
(prolog:<- (prolog:is-primitive cons))
(prolog:<- (prolog:is-primitive car))
(prolog:<- (prolog:is-primitive cdr))
(prolog:<- (prolog:is-primitive print))
(prolog:<- (prolog:is-primitive format))
