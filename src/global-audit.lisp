(in-package :iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. AUXILIARES (Las que NUNCA hay que borrar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-arity (form)
  (let ((count 0))
    (when (and (listp form) 
               (member (car form) '(defun defmacro))
               (listp (third form)))
      (dolist (a (third form))
        (when (member a '(&optional &rest &key &aux)) (return))
        (incf count)))
    count))

(defun cl-function-p (name-str)
  (let ((sym (find-symbol (string-upcase name-str) :common-lisp)))
    (and sym (fboundp sym))))

(defun clean-name-for-lisa (name-thing)
  (let* ((name-str (string-upcase (format nil "~A" name-thing)))
         (pos (search "::" name-str)))
    (if pos (subseq name-str (+ pos 2)) name-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. TEMPLATES Y REGLAS (Lógica de LISA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate sys-func ()
  (slot name)
  (slot arity))

(deftemplate sys-call ()
  (slot from)
  (slot to))

(deftemplate sys-goal ()
  (slot status))

(deftemplate sys-spof-score ()
  (slot func-name)
  (slot count)
  (slot processed-parents))


(defrule rule-cycle ()
  (sys-goal (status :active))
  (sys-call (from ?a) (to ?b))
  (sys-call (from ?b) (to ?a))
  (test (and (stringp ?a) (stringp ?b) (string< ?a ?b)))
  =>
  (lisa:assert (violation (rule-id "GLOBAL-01") (severity :warning)
                          (message (format nil "Ciclo: ~A <-> ~A" ?a ?b)) (score 10))))

(defrule rule-init-spof ()
  (sys-goal (status :active)) (sys-func (name ?n))
  (not (sys-spof-score (func-name ?n)))
  =>
  (lisa:assert (sys-spof-score (func-name ?n) (count 0) (processed-parents ()))))

(defrule rule-accumulate-spof ()
  (sys-goal (status :active))
  (sys-call (from ?p) (to ?c))
  (?f (sys-spof-score (func-name ?c) (count ?cnt) (processed-parents ?list)))
  (test (not (member ?p ?list :test #'equal)))
  =>
  (retract ?f)
  (lisa:assert (sys-spof-score (func-name ?c) (count (1+ (the fixnum ?cnt)))
                               (processed-parents (cons ?p ?list)))))

(defrule rule-report-spof-critical ()
  (sys-goal (status :active))
  (sys-spof-score (func-name ?n) (count ?cnt))
  (test (>= (the fixnum ?cnt) 3))
  =>
  (lisa:assert (violation (rule-id "GLOBAL-06") (severity :error)
                          (message (format nil "SPOF: '~A' tiene ~A dependientes." ?n ?cnt)) (score 15))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. PERSISTENCIA E INTEGRACIÓN CON EL GRAFO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-human-commit-with-audit (human-uuid violations)
  (let* ((vertex (find-vertex-by-uuid *human-history-graph* human-uuid))
         (data (when vertex (cl-graph:element vertex)))
         (total-score (reduce #'+ (mapcar #'fourth violations) :initial-value 0)))
    (when data
      (setf (getf data :global-score) total-score)
      (setf (getf data :audit-data) violations)
      (setf (cl-graph:element vertex) data)
      total-score)))

(defun run-global-audit (human-commit-uuid)
  (lisa:reset)
  (setf *audit-violations* nil)
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         (snapshot-table (make-hash-table :test 'equal)))
    (unless h-data (return-from run-global-audit nil))
    (format t "~%=== Ejecutando Auditoría Global ===~%")
    
    ;; Snapshot
    (loop for v in (cl-graph:topological-sort *human-history-graph*)
          for d = (cl-graph:element v)
          do (loop for a-uuid in (getf d :atomic-uuids)
                   do (let* ((a-v (find-vertex-by-uuid *atomic-history-graph* a-uuid))
                             (a-d (when a-v (cl-graph:element a-v)))
                             (s-name (getf a-d :symbol-name)))
                        (when s-name (setf (gethash s-name snapshot-table) a-d))))
          (when (equal (getf d :uuid) human-commit-uuid) (return)))

    ;; Carga
    (maphash (lambda (name data)
               (let ((clean-n (clean-name-for-lisa name))
                     (arity-v (calculate-arity (getf data :source-form))))
                 (eval `(lisa:assert (sys-func (name ,clean-n) (arity ,arity-v))))
                 (dolist (callee-raw (getf data :calls))
                   (let ((callee (clean-name-for-lisa callee-raw)))
                     (unless (cl-function-p callee)
                       (eval `(lisa:assert (sys-call (from ,clean-n) (to ,callee)))))))))
             snapshot-table)

    (eval `(lisa:assert (sys-goal (status :active))))
    (lisa:run)
    (update-human-commit-with-audit human-commit-uuid *audit-violations*)
    (report-global-results *audit-violations*)
    *audit-violations*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. DASHBOARD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun report-global-results (violations)
  "Imprime el Dashboard Global basado en las violaciones recolectadas por LISA."
  (let ((score (reduce #'+ (mapcar #'fourth violations) :initial-value 0))
        (sorted (sort (copy-list violations) #'> :key #'fourth)))
    
    (format t "~%~V@{~A~:*~}" 60 "=") 
    (format t "~%[ IISCV GLOBAL AUDIT DASHBOARD ]~%")
    (format t "~V@{~A~:*~}~%~%" 60 "-")
    
    (format t "ESTADO: ~A~%" 
            (cond ((> score 40) "RED - CRÍTICO")
                  ((> score 15) "YELLOW - AVISO")
                  (t "GREEN - ESTABLE")))
    (format t "RIESGO TOTAL: ~A pts~%" score)
    (format t "INCIDENCIAS:  ~A~%~%" (length violations))

    (format t "~10A | ~8A | ~A~%" "ID" "SCORE" "DESCRIPCIÓN")
    (format t "~V@{~A~:*~}~%" 60 "-")

    (dolist (v sorted)
      (format t "~10A | ~8A | ~A~%" 
              (third v)   ; ID (GLOBAL-XX)
              (fourth v)  ; Score
              (first v))) ; Mensaje
              
    (format t "~V@{~A~:*~}~%~%" 60 "=")))



(defun audit-current ()
  (if *current-human-commit*
      (run-global-audit *current-human-commit*)
      (format t "No hay commit.~%")))

(defun show-health-evolution ()
  "Muestra la tendencia del Global Score a través de los hitos."
  (format t "~%--- EVOLUCIÓN DE SALUD DEL PROYECTO ---~%")
  (format t "~30A | ~10A | ~A~%" "HITO" "SCORE" "TENDENCIA")
  (format t "~V@{~A~:*~}~%" 55 "-")
  
  (let ((last-score nil)
        ;; Obtenemos los nodos y los ordenamos por fecha/topología
        (nodes (cl-graph:topological-sort *human-history-graph*)))
    
    (dolist (node nodes)
      (let* ((data (cl-graph:element node))
             (score (getf data :global-score 0))
             (msg (or (getf data :message) "Sin mensaje"))
             (trend (cond ((null last-score) "INIT")
                          ((< score last-score) "MEJORANDO")
                          ((> score last-score) "EMPEORANDO")
                          (t "ESTABLE"))))
        
        ;; Recortamos el mensaje para que no rompa la tabla
        (let ((short-msg (if (> (length msg) 27) 
                             (concatenate 'string (subseq msg 0 27) "...")
                             msg)))
          (format t "~30A | ~10A | ~A~%" 
                  short-msg 
                  score 
                  trend))
        (setf last-score score))))
  (format t "~V@{~A~:*~}~%~%" 55 "-"))
