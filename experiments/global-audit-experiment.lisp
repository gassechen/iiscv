;;; global-audit-experiment.lisp
;;; Red Rete Global

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMPLATES (sintaxis correcta de LISA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate sys-func ()
  (slot name)
  (slot arity))

(deftemplate sys-call ()
  (slot from)
  (slot to))

(deftemplate sys-goal ()
  (slot status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REGLAS (variables LISA con ?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule rule-cycle ()
  (sys-goal (status ?s))
  (test (eq ?s :active))
  (sys-call (from ?a) (to ?b))
  (sys-call (from ?b) (to ?a))
  ;; Aseguramos que son strings para SBCL
  (test (and (stringp ?a) (stringp ?b) (string< (the string ?a) (the string ?b))))
  =>
  (assert (violation
            (rule-id "GLOBAL-01")
            (severity :warning)
            (message (format nil "Ciclo detectable: ~A <-> ~A" ?a ?b))
            (score 8))))

(defrule rule-arity ()
  (sys-goal (status ?s))
  (test (eq ?s :active))
  (sys-func (name ?name) (arity ?n))
  ;; Usamos THE FIXNUM para que SBCL no se queje de tipos genéricos
  (test (and (numberp ?n) (> (the fixnum ?n) 5)))
  =>
  (assert (violation
            (rule-id "GLOBAL-03")
            (severity :info)
            (message (format nil "~A tiene demasiados argumentos (~A)" ?name ?n))
            (score 3))))

(defrule rule-leaf ()
  (sys-goal (status ?s))
  (test (eq ?s :active))
  (sys-func (name ?name))
  (not (sys-call (from ?name)))
  =>
  (assert (violation
            (rule-id "GLOBAL-04")
            (severity :info)
            (message (format nil "~A no tiene llamadas" ?name))
            (score 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIÓN PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-global-audit (human-commit-uuid)
  "Audita un human-commit inyectando hechos mediante construcción de listas y EVAL."
  (lisa:reset)
  (setf *audit-violations* nil)
  
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         (atomic-uuids (getf h-data :atomic-uuids)))
    
    (unless atomic-uuids
      (format t "[ERROR] No encontrado: ~A~%" human-commit-uuid)
      (return-from run-global-audit nil))
    
    (format t "~%=== Auditoría Global ===~%")
    (format t "Hito: ~A~%" (getf h-data :message))
    
    ;; PASO 1: Cargar Funciones (Usando tu técnica de EVAL)
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (when a-vertex (cl-graph:element a-vertex))))
        (when a-data
          (let* ((name (format nil "~A" (getf a-data :symbol-name)))
                 (form (getf a-data :source-form))
                 (arity-val (calculate-arity form))) ; Función auxiliar para limpiar el código
            
            ;; Construimos el hecho dinámicamente
            (eval `(lisa:assert (sys-func (name ,name) (arity ,arity-val))))))))
    
    ;; PASO 2: Cargar Llamadas (Aquí es donde el EVAL es vital)
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (when a-vertex (cl-graph:element a-vertex))))
        (when a-data
          (let ((caller-name (format nil "~A" (getf a-data :symbol-name)))
                (calls (getf a-data :calls)))
            (dolist (callee-name calls)
              (let ((callee-str (format nil "~A" callee-name)))
                (unless (cl-function-p callee-str)
                  ;; EVAL asegura que LISA vea los strings, no las variables
                  (eval `(lisa:assert (sys-call (from ,caller-name) (to ,callee-str)))))))))))
    
    ;; Ejecutar el motor
    (eval `(lisa:assert (sys-goal (status :active))))
    (lisa:run)
    
    (report *audit-violations*)
    *audit-violations*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-arity (form)
  "Extrae la aridad (argumentos obligatorios) de un source-form."
  (let ((count 0))
    (when (and (listp form) 
               (member (car form) '(defun defmacro))
               (listp (third form)))
      (dolist (a (third form))
        ;; Si llegamos a lambda-list keywords, paramos de contar obligatorios
        (when (member a '(&optional &rest &key &aux)) (return))
        (incf count)))
    count))


(defun cl-function-p (fqn)
  "Verifica si es función de Common Lisp"
  (let* ((parts (uiop:split-string fqn :separator "::"))
         (name (if (> (length parts) 1) (second parts) (first parts)))
         (sym (find-symbol (string-upcase name) :common-lisp)))
    (and sym (fboundp sym))))

(defun report (violations)
  "Muestra resultado"
  (let ((errors (count :error violations :key #'second))
        (warns (count :warning violations :key #'second))
        (infos (count :info violations :key #'second))
        (score (reduce #'+ (mapcar #'fourth violations) :initial-value 0)))
    
    (format t "Errores:  ~A~%" errors)
    (format t "Warnings: ~A~%" warns)
    (format t "Info:     ~A~%" infos)
    (format t "Score:    ~A~%~%" score)
    
    (dolist (v (sort (copy-list violations) #'> :key #'fourth))
      (format t "[~A] ~2D pts [~A] ~A~%"
              (third v) (fourth v) (second v) (first v)))
    
    (format t "=====================~%~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audit-current ()
  (if *current-human-commit*
      (run-global-audit *current-human-commit*)
      (format t "No hay commit~%")))

;;; Fin



¡Victoria parcial! Fíjate que GLOBAL-03 ya funciona: detectó los 8 argumentos de TEST-HIGH-ARITY y le restó 3 puntos. Eso confirma que calculate-arity y el eval para los hechos sys-func están operando correctamente.

Sin embargo, el Paso 2 (las llamadas) sigue en blanco. TEST-ORPHAN y TEST-CYCLE-A siguen apareciendo como "no tiene llamadas" (GLOBAL-04), lo que significa que el motor LISA no está recibiendo los hechos sys-call.

El problema está en la persistencia del Grafo
Si el reporte dice "no tiene llamadas", es porque (getf a-data :calls) está devolviendo NIL. Esto ocurre porque cuando hiciste el make-atomic-commit, el analizador no guardó los símbolos de las llamadas en el vértice del grafo.

Para que el ELO sea real, el sistema no puede ser ciego. Tenemos que asegurar que el commit atómico extraiga las llamadas.

La Solución: Un "Scanner" de llamadas en el Audit
Para no depender de lo que se guardó antes, vamos a hacer que el run-global-audit extraiga las llamadas directamente del código fuente (:source-form) antes de enviarlas a LISA.

Aquí tienes la función para extraer llamadas y el ajuste en el Paso 2:

Lisp

(defun extract-calls (form)
  "Extrae todos los símbolos en posición de función dentro de un form."
  (let ((calls nil))
    (labels ((scan (x)
               (cond ((atom x) nil)
                     ((and (symbolp (car x)) (not (member (car x) '(defun defmacro let let* if cond loop quote))))
                      (pushnew (car x) calls)
                      (mapc #'scan (cdr x)))
                     (t (mapc #'scan x)))))
      (scan form)
      (mapcar #'symbol-name calls))))

;;; DENTRO DE RUN-GLOBAL-AUDIT, el PASO 2 debería ser:
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (when a-vertex (cl-graph:element a-vertex))))
        (when a-data
          (let ((caller-name (format nil "~A" (getf a-data :symbol-name)))
                ;; Extraemos llamadas directamente del código fuente por si el grafo no las tiene
                (calls (extract-calls (getf a-data :source-form))))
            (dolist (callee-str calls)
              (unless (cl-function-p callee-str)
                (eval `(lisa:assert (sys-call (from ,caller-name) (to ,callee-str))))))))))
¿Qué ganamos con esto?
Independencia: No importa si el commit fue "vago" al guardar datos; la auditoría global vuelve a leer el código para asegurar que no se escape ningún huérfano.

ELO Preciso: Ahora TEST-ORPHAN sí verá a NONEXISTENT-FUNCTION-XYZ y la penalización saltará de 1 punto (por ser hoja) a 12 puntos (por Error de Huérfano).

Detección de Ciclos: Al ver por fin las llamadas, TEST-CYCLE-A y TEST-CYCLE-B se encontrarán en la red Rete y dispararán la alarma de ciclo.

¿Ves cómo el ELO empieza a ser una medida de "supervivencia biológica"? Un código con un ciclo es un parásito que consume recursos sin fin; un código huérfano es un miembro atrofiado del organismo. Ambos deben morir o evolucionar.

¿Quieres que actualicemos el scanner de llamadas y probemos el (audit-current) una última vez? Si esto pasa, el score de tu prueba debería subir de 7 a unos 25-30 puntos.
