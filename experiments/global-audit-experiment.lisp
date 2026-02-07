;;; global-audit-experiment.lisp
;;; Red Rete Global

(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUXILIARES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Primero las Auxiliares (Para evitar Style-Warnings)
(defun calculate-arity (form)
  "Extrae la aridad de un source-form."
  (let ((count 0))
    (when (and (listp form) 
               (member (car form) '(defun defmacro))
               (listp (third form)))
      (dolist (a (third form))
        (when (member a '(&optional &rest &key &aux)) (return))
        (incf count)))
    count))

(defun cl-function-p (name-str)
  "Verifica si el nombre pertenece a Common Lisp básico."
  (let ((sym (find-symbol (string-upcase name-str) :common-lisp)))
    (and sym (fboundp sym))))

(defun report-global (violations)
  "Muestra el resultado consolidado."
  (let ((score (reduce #'+ (mapcar #'fourth violations) :initial-value 0)))
    (format t "~%=== Auditoría Global ===~%")
    (format t "Score Total de Diseño: ~A~%" score)
    (dolist (v (sort (copy-list violations) #'> :key #'fourth))
      (format t "[~A] ~2D pts [~A] ~A~%" 
              (third v) (fourth v) (second v) (first v)))
    (format t "=====================~%")))


(defun clean-name-for-lisa (name-thing)
  "Elimina el prefijo de paquete de un string o símbolo para que LISA compare peras con peras."
  (let* ((name-str (string-upcase (format nil "~A" name-thing)))
         (pos (search "::" name-str)))
    (if pos
        (subseq name-str (+ pos 2))
        name-str)))


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

(deftemplate sys-depends-on ()
  (slot parent)
  (slot child))

(deftemplate sys-spof-score ()
  (slot func-name)
  (slot count)
  (slot processed-parents))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule rule-init-spof ()
  (sys-goal (status :active))
  (sys-func (name ?n))
  (not (sys-spof-score (func-name ?n)))
  =>
  (assert (sys-spof-score (func-name ?n) (count 0) (processed-parents ()))))

(defrule rule-accumulate-spof ()
  "Suma solo si el padre no está en la lista de procesados."
  (sys-goal (status :active))
  (sys-depends-on (parent ?p) (child ?c))
  (?f (sys-spof-score (func-name ?c) (count ?cnt) (processed-parents ?list)))
  ;; El TEST es la clave: solo dispara si ?p NO está en la lista
  (test (not (member ?p ?list :test #'equal)))
  =>
  (retract ?f)
  (assert (sys-spof-score 
                 (func-name ?c) 
                 (count (1+ (the fixnum ?cnt)))
                 (processed-parents (cons ?p ?list)))))


(defrule rule-spof-violation-report ()
  (sys-goal (status :active))
  (sys-spof-score (func-name ?n) (count ?cnt))
  (test (> (the fixnum ?cnt) 2))
  =>
  ;; La lógica de no duplicar la hacemos acá en el Lisp del RHS
  (unless (member-if (lambda (v) 
                       (search ?n (slot-value v 'message)))
                     (iiscv::*audit-violations*)) ; Miramos tu lista global
    (lisa:assert (violation
                   (rule-id "GLOBAL-06")
                   (severity :error)
                   (message (format nil "SPOF: '~A' es crítico (~A dependientes)" ?n ?cnt))
                   (score 15)))))

(defrule rule-spof-violation-report ()
  "Esta solo mira el resultado final."
  (sys-goal (status :active))
  (sys-spof-score (func-name ?n) (count ?cnt))
  (test (> (the fixnum ?cnt) 2))
  ;; Evitamos duplicados en el reporte
  (not (violation (rule-id "GLOBAL-06") (message ?msg & (test (search ?n ?msg)))))
  =>
  (assert (violation
                 (rule-id "GLOBAL-06")
                 (severity :error)
                 (message (format nil "SPOF: '~A' es crítico (~A dependientes)" ?n ?cnt))
                 (score 15))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIÓN PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-global-audit (human-commit-uuid)
  "Audita un hito completo normalizando nombres para que la Red Rete conecte las llamadas."
  (lisa:reset)
  (setf *audit-violations* nil)
  
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         (atomic-uuids (getf h-data :atomic-uuids)))
    
    (unless atomic-uuids
      (format t "[ERROR] No se encontraron cambios para el hito: ~A~%" human-commit-uuid)
      (return-from run-global-audit nil))
    
    (format t "~%=== Iniciando Auditoría Global: ~A ===~%" (getf h-data :message))
    
    ;; PASO 1: Cargar Funciones (Nombres limpios)
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (when a-vertex (cl-graph:element a-vertex))))
        (when a-data
          (let* ((name (clean-name-for-lisa (getf a-data :symbol-name)))
                 (form (getf a-data :source-form))
                 (arity-val (calculate-arity form)))
            (eval `(lisa:assert (sys-func (name ,name) (arity ,arity-val))))))))
    
    ;; PASO 2: Cargar Llamadas (Asegurando coincidencia de strings)
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (when a-vertex (cl-graph:element a-vertex))))
        (when a-data
          (let ((caller (clean-name-for-lisa (getf a-data :symbol-name)))
                (calls (getf a-data :calls)))
            (dolist (callee-raw calls)
              (let ((callee (clean-name-for-lisa callee-raw)))
                (unless (cl-function-p callee)
                  ;; Aquí es donde ocurre la magia: caller y callee ahora son iguales 
                  ;; a los nombres definidos en el PASO 1.
                  (eval `(lisa:assert (sys-call (from ,caller) (to ,callee)))))))))))
    
    ;; PASO 3: Disparar el motor
    (eval `(lisa:assert (sys-goal (status :active))))
    (lisa:run)

    (update-human-commit-with-audit human-commit-uuid *audit-violations*)
    
    (report-global-results *audit-violations*)
    
    *audit-violations*)
  (values))


(defun report-global-results (violations)
  (let ((score (reduce #'+ (mapcar #'fourth violations) :initial-value 0))
        (sorted (sort (copy-list violations) #'> :key #'fourth)))
    
    (format t "~%~V@{~A~:*~}" 60 "=") 
    (format t "~%[ IISCV GLOBAL AUDIT DASHBOARD ]~%")
    (format t "~V@{~A~:*~}~%~%" 60 "-")
    
    (format t "ESTADO: ~A~%" (if (> score 15) "RED - CRÍTICO" (if (> score 5) "YELLOW - AVISO" "GREEN - ESTABLE")))
    (format t "RIESGO TOTAL: ~A pts~%" score)
    (format t "INCIDENCIAS:  ~A~%~%" (length violations))

    ;; Cabecera: ~10A (10 espacios a la izq), ~8A (8 espacios), etc.
    (format t "~10A | ~8A | ~A~%" "ID" "SCORE" "DESCRIPCIÓN")
    (format t "~V@{~A~:*~}~%" 60 "-")

    (dolist (v sorted)
      (format t "~10A | ~8A | ~A~%" 
              (third v)   ; ID (e.g., GLOBAL-01)
              (fourth v)  ; Score (e.g., 8)
              (first v))) ; Mensaje
              
    (format t "~V@{~A~:*~}~%~%" 60 "=")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun audit-current ()
  (if *current-human-commit*
      (run-global-audit *current-human-commit*)
      (format t "No hay commit~%")))

;;; Fin


(defun update-human-commit-with-audit (human-uuid violations)
  "Inyecta los resultados de la auditoría global en el nodo del hito humano."
  (let* ((vertex (find-vertex-by-uuid *human-history-graph* human-uuid))
         (data (when vertex (cl-graph:element vertex)))
         (total-score (reduce #'+ (mapcar #'fourth violations) :initial-value 0)))
    (when data
      ;; Añadimos el score y el desglose de violaciones al hito
      (setf (getf data :global-score) total-score)
      (setf (getf data :audit-data) violations)
      ;; Actualizamos el elemento en el grafo
      (setf (cl-graph:element vertex) data)
      total-score)))


(defun show-health-evolution ()
  "Muestra la tendencia del Global Score a través de los hitos sin errores de format."
  (format t "~%--- EVOLUCIÓN DE SALUD DEL PROYECTO ---~%")
  ;; Usamos números positivos para el ancho de columna
  (format t "~20A | ~10A | ~A~%" "HITO" "SCORE" "TENDENCIA")
  (format t "~V@{~A~:*~}~%" 55 "-")
  
  (let ((last-score nil))
    ;; Obtenemos los commits humanos ordenados
    (dolist (node (cl-graph:topological-sort *human-history-graph*))
      (let* ((data (cl-graph:element node))
             (score (getf data :global-score 0))
             (msg (or (getf data :message) "Sin mensaje"))
             (trend (cond ((null last-score) "INIT")
                          ((< score last-score) "🟢 MEJORANDO")
                          ((> score last-score) "🔴 EMPEORANDO")
                          (t "🟡 ESTABLE"))))
        
        ;; Recortamos el mensaje si es muy largo para que no rompa la tabla
        (let ((short-msg (if (> (length msg) 20) 
                             (concatenate 'string (subseq msg 0 17) "...")
                             msg)))
          (format t "~20A | ~10A | ~A~%" 
                  short-msg 
                  score 
                  trend))
        (setf last-score score))))
  (format t "~V@{~A~:*~}~%~%" 55 "-"))


;;;;;;;


(defun run-global-audit (human-commit-uuid)
  "Audita reconstruyendo el estado global (Snapshot) hasta el hito indicado."
  (lisa:reset)
  (setf *audit-violations* nil)
  
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         ;; Hash para guardar solo la última versión de cada función
         (snapshot-table (make-hash-table :test 'equal)))
    
    (unless h-data
      (format t "[ERROR] No se encontró el hito: ~A~%" human-commit-uuid)
      (return-from run-global-audit nil))

    (format t "~%=== Iniciando Auditoría Global: ~A ===~%" (getf h-data :message))

    ;; --- PASO 0: RECONSTRUCCIÓN DEL SNAPSHOT (Inspirado en tu dump-source-code) ---
    ;; Recorremos todos los hitos humanos en orden cronológico/topológico
    (loop for v in (cl-graph:topological-sort *human-history-graph*)
          for d = (cl-graph:element v)
          do (loop for a-uuid in (getf d :atomic-uuids)
                   do (let* ((a-v (find-vertex-by-uuid *atomic-history-graph* a-uuid))
                             (a-d (when a-v (cl-graph:element a-v)))
                             (s-name (getf a-d :symbol-name)))
                        (when s-name
                          ;; Pisamos con la versión más nueva de este símbolo
                          (setf (gethash s-name snapshot-table) a-d))))
          ;; Frenamos cuando llegamos al hito que estamos auditando
          (when (equal (getf d :uuid) human-commit-uuid) (return)))

    ;; --- PASO 1: CARGAR FUNCIONES DEL SNAPSHOT ---
    (maphash (lambda (name data)
               (let* ((clean-n (clean-name-for-lisa name))
                      (arity-val (calculate-arity (getf data :source-form))))
                 (eval `(lisa:assert (sys-func (name ,clean-n) (arity ,arity-val))))))
             snapshot-table)
    
    ;; --- PASO 2: CARGAR LLAMADAS DEL SNAPSHOT ---
    (maphash (lambda (name data)
               (let ((caller (clean-name-for-lisa name))
                     (calls (getf data :calls)))
                 (dolist (callee-raw calls)
                   (let ((callee (clean-name-for-lisa callee-raw)))
                     (unless (cl-function-p callee)
                       (eval `(lisa:assert (sys-call (from ,caller) (to ,callee)))))))))
             snapshot-table)
    
    ;; --- PASO 3: DISPARAR EL MOTOR ---
    (eval `(lisa:assert (sys-goal (status :active))))
    (lisa:run)

    ;; Persistencia y Reporte
    (update-human-commit-with-audit human-commit-uuid *audit-violations*)
    (report-global-results *audit-violations*)
    
    *audit-violations*))
