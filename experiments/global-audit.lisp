;; ;;### Implementación del Motor de Auditoría Global

;; Para esto, crearemos una función que "levante" una red de inferencia temporal con la "Verdad Curada".

;; #### 1. La Función Orquestadora: `run-global-logical-audit`

;; Esta función toma un hito humano, extrae todos sus átomos y los inyecta en una red LISA fresca.

;; ```lisp

(defun run-global-logical-audit (human-commit-uuid)
  "Ejecuta una red de inferencia 'one-shot' sobre el estado curado de un Human Commit."
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         (atomic-uuids (getf h-data :atomic-uuids)))
    
    (unless atomic-uuids
      (format t "Error: No hay átomos en este hito para auditar.~%")
      (return-from run-global-logical-audit nil))

    ;; --- INICIO DE LA RED ONE-SHOT ---
    (lisa:reset) ;; Limpiamos la memoria de trabajo
    
    (format t "~%[FORENSIC-AUDIT] Cargando ~A definiciones curadas...~%" (length atomic-uuids))
    
    ;; 1. Alimentar la red con la 'Verdad' del Grafo
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (cl-graph:element a-vertex)))
        ;; Inyectamos los datos atómicos como hechos de LISA
        (lisa:assert `(code-commit-analysis
                        (symbol-name ,(getf a-data :symbol-name))
                        (calls ',(getf a-data :calls))
                        (status :curated)
                        (expected-type ,(getf a-data :expected-type))
                        (has-side-effects-p ,(getf a-data :has-side-effects-p))))))

    ;; 2. Disparar el objetivo de validación global
    (lisa:assert '(goal (type :global-validation) (status :active)))

    ;; 3. Inferencia
    (lisa:run)

    ;; 4. Reporte de Resultados
    (let ((results iiscv::*audit-violations*))
      (format t "~%=== REPORTE DE INTEGRIDAD LÓGICA GLOBAL ===~%")
      (if results
          (dolist (v results) (format t "-> [~A] ~A~%" (third v) (first v)))
          (format t "No se detectaron inconsistencias lógicas en este hito.~%"))
      
      ;; 5. RESET FINAL (Mantenemos la imagen liviana)
      (lisa:reset)
      results)))
;; ```

;; #### 2. Reglas de Integridad Global (Cross-Function)

;; Estas reglas solo tienen sentido cuando LISA "ve" todo el programa a la vez.

;; **A. Referencias Huérfanas en el Hito:**
;; Si una función curada llama a algo que NO está incluido en este Human Commit ni existe en el sistema.

;; ```lisp

(defrule rule-global-orphan-call ()
  (goal (type :global-validation) (status :active))
  (code-commit-analysis (symbol-name ?caller) (calls ?calls))
  =>
  (dolist (callee ?calls)
    ;; Si el callee no existe como hecho en esta red 'one-shot'
    (unless (lisa:find-fact-instance 'code-commit-analysis (symbol-name callee))
      (assert (violation (rule-id "GLOBAL-01")
                         (severity :error)
                         (message (format nil "Inconsistencia de Hito: '~A' llama a '~A', pero '~A' no forma parte de este commit curado." ?caller callee callee)))))))
;; ```

;; **B. Contradicción de Efecto de Sitio:**
;; Si una función pura (sin side effects) llama a una que SÍ tiene side effects, podrías estar rompiendo la transparencia referencial de tu arquitectura.

;; ```lisp
 (defrule rule-global-purity-violation ()
   (code-commit-analysis (symbol-name ?pure-fn) (has-side-effects-p nil) (calls ?calls))
   (code-commit-analysis (symbol-name ?dirty-fn) (has-side-effects-p t))
   (test (member ?dirty-fn ?calls :test #'equal))
   =>
   (assert (violation (rule-id "GLOBAL-02")
                      (severity :warning)
                      (message (format nil "Violación de Pureza: '~A' (declarada pura) depende de '~A' (que tiene efectos de sitio)." ?pure-fn ?dirty-fn)))))
;; ```

;; ### Por qué esto es superior:

;; 1.  **Ahorro de Memoria:** Al ser "One-Shot", la red LISA solo crece durante el análisis. Una vez que termina, `lisa:reset` libera todos los objetos del motor, manteniendo tu imagen Lisp limpia de "hechos" viejos.
;; 2.  **Determinismo:** El archivo que generes con `make-human-history-file` después de este análisis estará **garantizado logícamente**. No habrá sorpresas de "falta una función" o "el tipo no coincide".
;; 3.  **Forense:** Puedes guardar el resultado de este análisis global *dentro* del nodo del Human Commit. Así, el registro dirá: *"Este hito fue validado lógicamente el 24/05/2024 y pasó 150 reglas de integridad"*.

;; **¿Te gustaría que diseñemos un "Walker" más profundo que extraiga los tipos de las funciones (`(declare (type ...))`) para que el análisis global sea de tipado fuerte?**


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Arquitectura de la Red Rete One-Shot
;; Para no interferir con la red liviana que usas en el REPL, podemos instanciar un motor de inferencia temporal.

(defun run-systemic-logic-audit ()
  "Crea una red Rete temporal, carga todas las definiciones humanas (curadas) 
   y ejecuta un análisis de integridad lógica global."
  (let ((violations nil))
    ;; 1. Crear/Resetear un motor de inferencia limpio para este proceso
    (lisa:make-inference-engine) 
    (setf *audit-violations* nil)

    ;; 2. Cargar el 'Corte de Verdad' (Human Commits)
    (format t "~%--- Iniciando Auditoría Lógica Sistémica ---~%")
    (cl-graph:iterate-vertices *human-history-graph*
      (lambda (v)
        (let* ((human-data (cl-graph:element v))
               ;; Obtenemos la última versión atómica de cada función en este hito
               (atomic-data (get-source-form-by-uuid (getf human-data :last-atomic-uuid))))
          ;; Asertamos cada función curada en la red temporal
          (iiscv::assert-logic-fact atomic-data))))

    ;; 3. Inyectar Meta Global de Auditoría
    (lisa:assert `(goal (type systemic-validation) (status active)))

    ;; 4. Ejecutar Razonamiento
    (lisa:run)
    
    ;; 5. Capturar resultados y limpiar
    (setf violations *audit-violations*)
    (lisa:reset) ; Liberamos memoria inmediatamente
    
    (format t "Auditoría completada. ~D violaciones encontradas.~%" (length violations))
    violations))


;; 2. Reglas Sistémicas (El "Poder" de la red cargada)Al tener todas las definiciones en la memoria de Rete al mismo tiempo, podemos activar reglas que antes eran imposibles:A. Detección de Recursión Infinita IndirectaLisa puede detectar si $A \rightarrow B \rightarrow C \rightarrow A$, algo que un analizador atómico nunca vería.

(defrule rule-systemic-circular-dependency
   (goal (type systemic-validation))
   (dependency (caller ?a) (callee ?b))
   (dependency (caller ?b) (callee ?c))
   (dependency (caller ?c) (callee ?a))
   =>
   (assert (violation (rule-id "GLOBAL-LOGIC-01")
                      (severity :critical)
             (message (format nil "Ciclo lógico detectado: ~A -> ~B -> ~C -> ~A. Posible recursión infinita." ?a ?b ?c)))))

;; B. Inconsistencia de Firmas (Arity Mismatch Global)
;; Ahora que tenemos todas las funciones "vivas", podemos validar si alguien llama a una función con el número incorrecto de argumentos.

(defrule rule-systemic-arity-check
   (goal (type systemic-validation))
   (function-call (caller ?caller) (callee ?callee) (args-count ?count))
   (function-definition (name ?callee) (expected-args ?expected))
   (test (/= ?count ?expected))
   =>
   (assert (violation (rule-id "GLOBAL-LOGIC-02")
                      (severity :error)
                      (message (format nil "Error de Aridad: '~A' llama a '~A' con ~D argumentos, pero se esperan ~D." 
                                       ?caller ?callee ?count ?expected)))))

;;3. Implementación de assert-logic-fact
;;Esta función auxiliar prepara los datos para la red sistémica:

(defun assert-logic-fact (atomic-data)
  "Extrae la estructura lógica para la red sistémica."
  (let ((name (getf atomic-data :symbol-name))
        (calls (getf atomic-data :calls))
        (args (getf atomic-data :arg-list))) ; Necesitaremos extraer la lista de argumentos
    (lisa:assert `(function-definition (name ,name) (expected-args ,(length args))))
    (dolist (call calls)
      (lisa:assert `(function-call (caller ,name) (callee ,call) (args-count ,(get-call-arity atomic-data call)))))))

;;1. Preparación de los Templates Sistémicos
;;En lisa-rules.lisp, necesitamos templates que permitan a Rete hacer "Joins" entre funciones.

(deftemplate systemic-function ()
  (slot name)
  (slot arg-list)
  (slot arity))

(deftemplate systemic-call ()
  (slot caller)
  (slot callee)
  (slot arity-used))

;;2. La Función de Auditoría Global (en main.lisp)
;;Esta función orquestará la red temporal. Fíjate que usamos lisa:make-inference-engine para asegurar que estamos en una red limpia y luego la reseteamos.

(defun run-systemic-logic-audit ()
  "Realiza una auditoría profunda de todos los Human Commits actuales."
  (let ((all-violations nil))
    ;; 1. Crear motor limpio y resetear lista de violaciones
    (lisa:make-inference-engine)
    (setf *audit-violations* nil)
    
    (format t "~%>>> Iniciando Auditoría Sistémica One-Shot...~%")

    ;; 2. Poblar la red con la 'Verdad' del Grafo Humano
    (cl-graph:iterate-vertices *human-history-graph*
      (lambda (v)
        (let* ((human-node (cl-graph:element v))
               (last-uuid (getf human-node :last-atomic-uuid))
               (form (get-source-form-by-uuid last-uuid)))
          (when form
            (iiscv::assert-systemic-facts form)))))

    ;; 3. Inyectar Meta de Validación
    (lisa:assert `(goal (type systemic-validation) (status active)))

    ;; 4. Disparar Inferencia
    (lisa:run)
    
    ;; 5. Capturar y Limpiar
    (setq all-violations *audit-violations*)
    (lisa:reset) ; Volvemos a dejar Lisa liviana
    
    (display-audit-report all-violations)
    all-violations))

;;3. Extracción de Hechos Sistémicos (en lisa-rules-aux-fn.lisp)
;;Necesitamos una función que "desmonte" el código curado en hechos que Lisa entienda.

(defun assert-systemic-facts (form)
  "Traduce un form de Lisp a hechos de arquitectura para Lisa."
  (let* ((name (extract-symbol-name form))
         (args (second form)) ;; Asumiendo (defun name (args) ...)
         (package (package-name (symbol-package (if (listp name) (second name) name))))
         (full-name (format nil "~A::~A" package name)))
    
    ;; Hecho de definición
    (lisa:assert `(systemic-function (name ,full-name) 
                                     (arg-list ',args) 
                                     (arity ,(length args))))
    
    ;; Hechos de llamadas (reutilizamos extract-calls que hicimos antes)
    (let ((calls-with-arity (extract-calls-with-arity form)))
      (dolist (c calls-with-arity)
        (lisa:assert `(systemic-call (caller ,full-name) 
                                     (callee ,(car c)) 
                        (arity-used ,(cdr c))))))))

;;4. Reglas de Inteligencia Sistémica (en lisa-rules.lisp)
;;Aquí es donde ocurre la magia. Estas reglas solo funcionan cuando tienes todo el programa cargado.

;;A. Detección de Recursión Infinita Directa/Indirecta


(defrule rule-systemic-circularity
  (goal (type systemic-validation))
  (systemic-call (caller ?a) (callee ?b))
  (systemic-call (caller ?b) (callee ?a))
  =>
  (assert (violation (rule-id "SYS-LOGIC-01")
                     (severity :critical)
            (message (format nil "Recursión mutua detectada entre '~A' y '~A'." ?a ?b)))))

;;B. Validación de Aridad Global (Adiós errores en Runtime)

(defrule rule-systemic-arity-mismatch
  (goal (type systemic-validation))
  (systemic-call (caller ?caller) (callee ?callee) (arity-used ?used))
  (systemic-function (name ?callee) (arity ?expected))
  (test (/= ?used ?expected))
  =>
  (assert (violation (rule-id "SYS-LOGIC-02")
                     (severity :error)
                     (message (format nil "Error de Aridad Global: '~A' llama a '~A' con ~D args, pero requiere ~D." 
                                      ?caller ?callee ?used ?expected)))))


;;1. Preparación de los Templates Sistémicos
;;En lisa-rules.lisp, necesitamos templates que permitan a Rete hacer "Joins" entre funciones. Estos no se limpian hasta que termine la auditoría completa.

(deftemplate systemic-function ()
  (slot name)
  (slot arg-list)
  (slot arity))

(deftemplate systemic-call ()
  (slot caller)
  (slot callee)
  (slot arity-used))

;;2. La Función de Auditoría Global (en main.lisp)
;;Esta función orquestará la red temporal. Usamos lisa:make-inference-engine para asegurar un entorno limpio y, al final, ejecutamos un reporte de "Salud del Sistema".

(defun run-systemic-logic-audit ()
  "Realiza una auditoría profunda de todos los Human Commits actuales."
  (let ((all-violations nil))
    ;; 1. Crear motor limpio (One-Shot)
    (lisa:make-inference-engine)
    (setf *audit-violations* nil)
    
    (format t "~%>>> Iniciando Auditoría Sistémica One-Shot...~%")

    ;; 2. Poblar la red con la 'Verdad' del Grafo Humano (Curado)
    (cl-graph:iterate-vertices *human-history-graph*
      (lambda (v)
        (let* ((human-node (cl-graph:element v))
               (last-uuid (getf human-node :last-atomic-uuid))
               (form (get-source-form-by-uuid last-uuid)))
          (when form
            (iiscv::assert-systemic-facts form)))))

    ;; 3. Inyectar Meta de Validación y Correr
    (lisa:assert `(goal (type systemic-validation) (status active)))
    (lisa:run)
    
    ;; 4. Capturar, Limpiar y Reportar
    (setq all-violations *audit-violations*)
    (lisa:reset) 
    
    (generate-health-report all-violations)
    all-violations))


;;3. Extracción de Hechos Sistémicos (en lisa-rules-aux-fn.lisp)
;;Esta función "desmonta" el código curado en hechos que Lisa puede cruzar para detectar errores de integridad.

(defun assert-systemic-facts (form)
  "Traduce un form de Lisp a hechos de arquitectura para Lisa."
  (let* ((name (extract-symbol-name form))
         (args (second form)) ;; Lista de argumentos: (defun name (args) ...)
         (full-name (format nil "~A" name)))
    
    (lisa:assert `(systemic-function (name ,full-name) 
                                     (arg-list ',args) 
                                     (arity ,(length args))))
    
    ;; Usamos el extractor de llamadas que desarrollamos antes
    (let ((calls (extract-calls form)))
      (dolist (c calls)
        ;; Aquí podríamos extraer la aridad usada en cada llamada específica
        (lisa:assert `(systemic-call (caller ,full-name) (callee ,c)))))))

;;4. Reglas de Inteligencia Sistémica (en lisa-rules.lisp)
;;Estas reglas solo funcionan cuando tienes todo el programa cargado en la red Rete.

;;A. Detección de Ciclos de Dependencia

(defrule rule-systemic-circularity
  (goal (type systemic-validation) (status active))
  (systemic-call (caller ?a) (callee ?b))
  (systemic-call (caller ?b) (callee ?a))
  =>
  (assert (violation (rule-id "SYS-LOGIC-01")
                     (severity :critical)
            (message (format nil "Recursión mutua/Ciclo detectado entre '~A' y '~A'." ?a ?b)))))

;;B. Símbolos Huérfanos (Llamadas a funciones inexistentes)

(defrule rule-systemic-orphan-call
  (goal (type systemic-validation) (status active))
  (systemic-call (caller ?caller) (callee ?callee))
  ;; No existe una definición para esa llamada en los human commits
  (not (systemic-function (name ?callee)))
  (test (not (fboundp (find-symbol (string-upcase ?callee))))) ; Tampoco es de CL
  =>
  (assert (violation (rule-id "SYS-LOGIC-03")
                     (severity :error)
            (message (format nil "Referencia Huérfana: '~A' llama a '~A', pero no está definida." ?caller ?callee)))))


;;El Reporte de Salud (Nota de 0 a 100)
;;Para cerrar con broche de oro, añadimos una métrica que resuma el estado del proyecto:

(defun generate-health-report (violations)
  "Calcula el índice de calidad ISO 25000 basado en las violaciones detectadas."
  (let* ((total-score 100)
         (categories '((:mantenibilidad . 0) (:fiabilidad . 0) (:integridad . 0)))
         (total-v (length violations)))
    
    ;; Clasificamos las violaciones por impacto ISO
    (dolist (v violations)
      (let ((severity (lisa:get-slot-value v 'severity))
            (rule-id (lisa:get-slot-value v 'rule-id)))
        (cond 
          ((search "SYS-01" rule-id) (incf (cdr (assoc :mantenibilidad categories)) 15))
          ((search "SYS-02" rule-id) (incf (cdr (assoc :fiabilidad categories)) 10))
          ((search "SYS-03" rule-id) (incf (cdr (assoc :integridad categories)) 20)))))

    (format t "~%==================================================")
    (format t "~%   REPORTE DE CALIDAD SISTÉMICA (ISO 25000)")
    (format t "~%==================================================")
    
    ;; Mostramos cada pilar
    (loop for (cat . penalty) in categories do
      (let ((cat-score (- 100 penalty)))
        (format t "~% [~A]: ~D/100 ~A" 
                (string-capitalize (symbol-name cat))
                (max 0 cat-score)
                (cond ((>= cat-score 90) "✔ EXCELENTE")
                      ((>= cat-score 70) "⚠ ACEPTABLE")
                      (t "✘ RIESGO CRÍTICO")))))
    
    (format t "~%--------------------------------------------------")
    (format t "~% Nota Global del Sistema: ~D/100" 
            (max 0 (- 100 (reduce #'+ (mapcar #'cdr categories)))))
    (format t "~%==================================================~%")))


;;2. Nuevas Reglas en systemic-rules.lisp
;;Añadiremos una regla de Mantenibilidad para detectar "Acoplamiento Excesivo" (funciones que dependen de demasiadas otras funciones), lo cual es una métrica clave de la ISO 25000.

;; REGLA: Acoplamiento Crítico (Mantenibilidad)
(defrule rule-systemic-high-coupling ()
  (goal (type systemic-validation) (status active))
  ;; Buscamos funciones que tengan más de 7 dependencias (Número de Miller)
  (systemic-function (name ?f))
  (test (> (length (iiscv::get-all-calls-for ?f)) 7))
  =>
  (assert (violation (rule-id "SYS-ISO-MAINT-01")
                     (severity :warning)
                     (message (format nil "Acoplamiento Alto en '~A': Depende de demasiados componentes, dificultando su mantenimiento." ?f)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;AG
;;;
;;1. Estructura de Datos para el ELO
;;Siguiendo tu arquitectura, el deftemplate para los scores debe vincularse al símbolo para rastrear su linaje y su éxito competitivo.

(deftemplate symbol-elo-rating ()
  (slot symbol-name)          ;; El nombre de la "especie" o función.
  (slot current-elo)           ;; Valor dinámico (ej. inicia en 1000).
  (slot generation)           ;; Cuántas mutaciones lleva el linaje.
  (slot matches-played)       ;; Cuántas veces fue auditado/testeado.
  (slot last-penalty-reason))  ;; Buffer para que el "Maestro" guarde la última lecció

;;2. El Lisp-Critic como Maestro Dual
;;El slot style-critiques que ya tenés en code-commit-analysis  es el canal de comunicación. La regla debe operar en dos dimensiones:

;;Dimensión 1: El Castigo (Selección Natural) Si el Critic encuentra código no idiomático (como usar setq donde iría un let), se dispara una regla que resta ELO. Esto reduce las chances de que ese código sea el "padre" de la siguiente generación.


;;Dimensión 2: La Enseñanza (Guía de Mutación) La sugerencia (ej: "Use INCF instead" ) se almacena como un metadato de mutación. Cuando el algoritmo genético cree un nuevo individuo basado en ese, el "Maestro" le da prioridad a las mutaciones que apliquen el cambio sugerido.

;;3. Reglas de Ajuste de ELO
;;Podemos definir reglas en LISA que reaccionen a la severidad de los hechos que ya tenés:

;; Regla conceptual para penalización severa
(defrule adjust-elo-on-error ()
  (violation (severity :error) (rule-id ?id) (message ?msg))
  ?elo <- (symbol-elo-rating (symbol-name ?name))
  =>
  ;; Restamos puntos masivos por errores de integridad lógica o seguridad [cite: 16, 29]
  (modify ?elo (current-elo (- (slot-value ?elo 'current-elo) 50))
               (last-penalty-reason ?msg)))

;; Regla para el "Maestro" Lisp-Critic
(defrule adjust-elo-on-style ()
  (violation (rule-id "IDIOMATIC-01") (message ?msg))
  ?elo <- (symbol-elo-rating (symbol-name ?name))
  =>
  ;; La crítica de estilo es una penalización menor, pero educativa [cite: 26]
  (modify ?elo (current-elo (- (slot-value ?elo 'current-elo) 10))
    (last-penalty-reason ?msg)))


;; El ELO actúa como un "Certificado de Calidad" numérico. En lugar de que el humano lea párrafos de justificación técnica, el human-commit presenta una métrica de reputación del código. Si una función llega al commit final con un ELO de 1400, sabemos que sobrevivió a miles de pruebas y críticas de estilo sin despeinarse.



;; Este enfoque convierte el historial de cambios en un tablero de posiciones de ingeniería.

;; Estructura del Human-Commit con Métricas de ELO
;; Cuando el sistema genera el informe final, la sección de definiciones debería verse así:

;; PROYECTO: IOE-APP-TODOLIST ESTADO: PERSISTIDO (Snapshot OK)

;; DEFINICIONES EXPORTADAS: * ADICIONAR-TAREA [ELO: 1350] - Linaje puro, 0 penalizaciones. * BORRAR-TAREA-POR-ID [ELO: 1120] - Penalización leve por complejidad ciclomática (CC: 8). * LISTAR-PENDIENTES [ELO: 1280] - Optimizado por el Maestro (Lisp-Critic). 





;; RESUMEN GENÉTICO: > Generaciones totales: 450. Tasa de supervivencia: 12%. 

;; Implementación Conceptual del "Maestro" en los Facts
;; Para que esto funcione, el sistema debe inyectar la información de las reglas de LISA directamente en el acumulador del commit.


;; El Template del Score: Definimos el symbol-elo-rating para rastrear el valor acumulado de cada definición analizada.



;; Regla de Feedback Dual: El "Maestro" detecta una violación de estilo (Regla IDIOMATIC-01) y, mientras penaliza el ELO, genera un "log de aprendizaje" que se adjunta al commit.


;; Valor Condensado: El ELO final del code-commit-analysis se calcula restando los pesos de las violaciones de mantenibilidad, seguridad y estilo detectadas durante la auditoría.



;; ¿Por qué es información valiosa?

;; Confianza Inmediata: Un humano puede ver que la lógica de "Seguridad" (Regla 3.1) tiene un ELO altísimo, lo que garantiza que no hubo intentos de ejecución de comandos inseguros en ninguna generación.


;; Detección de Deuda Técnica: Si una función aceptada tiene un ELO bajo (ej. 800), el humano sabe inmediatamente que es un punto de riesgo (quizás funciones muy largas o con parámetros sin usar) que requiere atención en la siguiente fase evolutiva.



;; Adiós a la Prosa Innecesaria: El número resume el cumplimiento de las 11 categorías de reglas que ya tienes en tu archivo (Mantenibilidad, Fiabilidad, Seguridad, Eficiencia, etc.).
