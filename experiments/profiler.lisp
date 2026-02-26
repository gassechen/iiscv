(in-package :iiscv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;MULTIMOTORES


;; ;; Creamos los motores una sola vez
;; (defvar *engine-atomo* (lisa:make-inference-engine))
;; (defvar *engine-perf* (lisa:make-inference-engine))
;; (defvar *engine-global* (lisa:make-inference-engine))

;; ;; Ejemplo de cómo envolver lo que ya tenés
;; (lisa:with-inference-engine (*engine-atomo*)
;;    (analyze-commit-and-assert ...) ;; Tu función tal cual está
;; )


;; (defun inyectar-reporte-final (nombre stats)
;;   (lisa:with-inference-engine (*engine-perf*)
;;      ;; Aquí va TU código actual de inyectar reporte
;;      ;; El (lisa:reset) que hagas acá no borrará el grafo global
;;      ...
;;     ))


;; (defun run-global-audit (human-commit-uuid)
;;   (lisa:with-inference-engine (*engine-global*)
;;     ;; Si le sacás el (lisa:reset) de adentro, esta red persiste.
;;     ;; El resto de tu lógica (Snapshot, Carga, Inferencia) queda igual.
;;     ...
;;   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun benchmark-con-registro (nombre-atomo funcion args &key (iteraciones 10) (muestras 30))
;;   (let* ((resultados '())
;;          (nombre-str (format nil "~A" nombre-atomo))
;;          (raiz-proyecto (asdf:system-source-directory :iiscv))
;;          (dir-logs (uiop:merge-pathnames* "perf-log/" raiz-proyecto))
;;          (archivo (uiop:merge-pathnames* (format nil "~A-perf.log" nombre-str) dir-logs)))
    
;;     (ensure-directories-exist archivo)

;;     (with-open-file (stream archivo :direction :output :if-exists :append :if-does-not-exist :create)
;;       (format t "~%[IISCV] Generando evidencia física en: ~A" (uiop:native-namestring archivo))
;;       ;; Escribimos cabecera solo si el archivo es nuevo
;;       (when (= 0 (file-length stream))
;;         (format stream "timestamp,muestreo,iteraciones,mediana_bloque~%"))

;;       (dotimes (m muestras)
;;         (let ((t-inicio (get-internal-run-time)))
;;           (dotimes (i iteraciones)
;;             (apply (if (symbolp funcion) (symbol-function funcion) funcion) args))
;;           (let* ((t-final (get-internal-run-time))
;;                  (delta (- t-final t-inicio))
;;                  (promedio-bloque (/ (float delta) iteraciones)))
;;             (push promedio-bloque resultados)
;;             ;; GUARDAMOS LA EVIDENCIA
;;             (format stream "~A,~D,~D,~F~%" (get-universal-time) m iteraciones promedio-bloque)))))

;;     (let ((mediana (alexandria:median resultados))
;;           (desvio (alexandria:standard-deviation resultados)))
;;       (list :mediana mediana :desvio desvio :nombre nombre-str :archivo (uiop:native-namestring archivo)))))




(defun benchmark-con-registro (nombre-atomo funcion args &key (iteraciones 10000))
 (let ((muestras '()))
   ;; Tomamos 30 muestras de bloques para tener una campana de Gauss limpia
   (dotimes (m 30)
     (let ((t-inicio (get-internal-run-time)))
       (dotimes (i iteraciones)
         (apply (if (symbolp funcion) (symbol-function funcion) funcion) args))
       (let ((delta (- (get-internal-run-time) t-inicio)))
         (push delta muestras))))
    
   (let ((mediana (alexandria:median muestras))
         (desvio (alexandria:standard-deviation muestras)))
     (format t "~%--- NUEVA ESTADÍSTICA ---~%")
     (format t "Mediana: ~A | Desvío: ~,4F~%" mediana desvio)
     ;; Retornamos la lista para actualizar la variable
     (list :mediana mediana :desvio desvio :nombre (format nil "~A" nombre-atomo)))))


(deftemplate perf-report ()
  (slot nombre)
  (slot uuid)        
  (slot source-code) 
  (slot mediana)
  (slot desvio)
  (slot estabilidad)
  (slot log-path))


(defrule rule-performance-instability ()
  "Regla de performance integrada al flujo de LISA e IISCV."
  ;; 1. Buscamos el análisis (sin importar el status, para que funcione post-commit)
  (code-commit-analysis (symbol-name ?name))
  
  ;; 2. Buscamos el reporte de performance
  (perf-report (nombre ?n-perf) (estabilidad ?est) (mediana ?med))
  
  ;; 3. Validamos que sea el mismo átomo (Símbolo vs String)
  (test (string-equal (format nil "~A" ?name) (format nil "~A" ?n-perf)))
  
  ;; 4. Umbral de inestabilidad física
  (test (and (numberp ?est) 
           (> ?est 0.20)
           (> ?med 0.0001)))
  =>
  ;; 5. Generamos el hecho 'violation'. 
  ;; El rule-bridge-violations se encargará de hacer el PUSH y el RETRACT.
  (assert (violation (rule-id "PERF-1")
                    (severity :warning)
                    (message (format nil "Performance inestable detectada en '~a' (CV: ~,2f)." ?name ?est))
                    (score 15)))
  (format t "~%[LISA-PERF] Violación generada para ~A. El Bridge la procesará." ?name))



;; Regla de "Degradación por Presión" (Oversampling)
(defrule rule-perf-heavy-tail ()
  "Detecta funciones que tienen picos de latencia inaceptables (Heavy Tail)."
  (perf-report (nombre ?n) (mediana ?med) (desvio ?std))
  ;; Si el desvío es más grande que la mediana, el proceso es impredecible
  (test (> ?std ?med))
  =>
  (assert (violation (rule-id "PERF-2")
                    (severity :error)
                    (message (format nil "Inconsistencia Crítica en '~A': El desvío (~,2f) supera la mediana. Comportamiento errático." ?n ?std))
            (score 25))))


;;2. Regla de "Atomo Frío" (Zero-Performance)
(defrule rule-perf-too-fast-to-be-true ()
  (perf-report (nombre ?n) (mediana ?med))
  (test (<= ?med 0))
  =>
  (assert (violation (rule-id "PERF-3")
                    (severity :warning)
                    (message (format nil "Lectura sospechosa en '~A': Mediana = 0. ¿La función está haciendo algo realmente?" ?n))
                    (score 5))))




;;3. La Regla "Muro de Hierro": Curation Veto
(defrule rule-perf-curation-veto ()
  "Si el átomo está en la red global y la performance es mala, penaliza con 40 puntos."
  (perf-report (nombre ?n-perf) (estabilidad ?cv))
  ;; Buscamos la función en la red global que ya cargaste
  (sys-func (name ?name))
  ;; Normalizamos a string y mayúsculas para que el match sea exacto
  (test (and (string-equal (format nil "~A" ?name) (format nil "~A" ?n-perf))
             (> ?cv 0.30))) 
  =>
  (lisa:assert (violation (rule-id "PERF-VETO")
                          (severity :error)
                          (message (format nil "VETO FÍSICO: '~A' es inestable en el despliegue global." ?name))
                          (score 40))))


(defrule rule-global-perf-critical-spof ()
  (sys-spof-score (func-name ?n) (count ?cnt))
  (perf-report (nombre ?n) (estabilidad ?cv))
  (test (and (>= ?cnt 3) (> ?cv 0.20)))
  =>
  (assert (violation (rule-id "GLOBAL-PERF-01")
                    (severity :error)
                    (message (format nil "PELIGRO: El SPOF '~A' es físicamente inestable. Riesgo de cuello de botella global." ?n))
                    (score 30))))


(defrule rule-persist-performance-evidence ()
  "Registra la evidencia usando el UUID que ya viene certificado en el reporte."
  ;; 1. Extraemos el UUID directamente del reporte (F-30)
  (?report (perf-report (uuid ?uuid-reporte) 
                        (nombre ?n) 
                        (mediana ?m) 
                        (desvio ?d) 
                        (estabilidad ?e)
			(source-code ?src)))
  
  ;; 2. Solo verificamos que exista la función en la red para validar el contexto
  (sys-func (name ?name-sym))
  (test (string-equal (format nil "~A" ?n) (format nil "~A" ?name-sym)))
  
  ;; 3. Evitamos registrar si el UUID es nulo por alguna razón
  (test (not (null ?uuid-reporte)))
  =>
  ;; 4. Usamos ?uuid-reporte, que es el que vimos que vale "BB1C6447..."
  (registrar-evidencia-dinamica ?uuid-reporte (list :mediana ?m :desvio ?d :estabilidad ?e) ?src)
  
  (format t "~%[LISA-AUDIT] Evidencia física del UUID ~A certificada." ?uuid-reporte))


;;(defrule rule-perf-orphan ()
;;  "Evita el falso positivo si la red global (sys-func) está cargada."
;;  (perf-report (nombre ?n))
  ;; Si NO existe el análisis atómico...
;;  (not (code-commit-analysis (symbol-name ?n)))
  ;; ...Y TAMPOCO existe la función en la red global
;;  (not (sys-func (name ?n)))
;;  =>
;;  (format t "~%[!] Advertencia: Recibida performance de '~A' pero no existe registro previo en ninguna red." ?n))

(defun inyectar-reporte-final (nombre-atomo stats)
  "Recupera la identidad (UUID) y el cuerpo (SOURCE) del átomo antes de inyectar a LISA."
  (let* (;; Normalizamos el nombre para la búsqueda de UUID
         (nombre-sym (if (symbolp nombre-atomo) nombre-atomo (intern (string-upcase nombre-atomo))))
         (uuid (get-last-uuid-by-name nombre-sym)) ;; Ahora sí pasamos el símbolo correcto
         (source (when uuid (get-source-form-by-uuid uuid)))
         (med (getf stats :mediana))
         (std (getf stats :desvio))
         (cv (if (or (null med) (zerop med)) 0 (/ std med)))
         (fact-data
           `(perf-report 
             (nombre ,(format nil "~A" nombre-sym))
             (uuid ,uuid)
             (source-code ',source)
             (mediana ,med)
             (desvio ,std)
             (estabilidad ,cv)
             (log-path ,(getf stats :archivo)))))
    
    (if (null uuid)
        (format t "~%[!] ERROR FORENSE: No se encontró UUID para ~A." nombre-sym)
        (progn
          (eval `(lisa:assert ,fact-data))
          (lisa:run)
          (format t "~%[IISCV-PERF] Peritaje inyectado para UUID: ~A" uuid)))))


(defvar *registro-peritajes-1* (make-hash-table :test 'equal)
  "Almacena evidencias dinámicas indexadas por el UUID del átomo.")


(defun registrar-evidencia-dinamica (uuid reporte-perf &optional source)
  "Vincula el resultado y muestra el veredicto físico en pantalla."
  (let* ((estabilidad (getf reporte-perf :estabilidad))
         ;; Determinamos si es un fallo basado en tu umbral de inestabilidad (20%)
         (is-fail (and estabilidad (> estabilidad 0.20))))
    
    (push (list :timestamp (get-universal-time)
                :data reporte-perf
                :evidence-source source) 
          (gethash uuid *registro-peritajes-1*))

    ;; Feedback visual para el humano en el REPL
    (format t "~%--------------------------------------------------")
    (if is-fail
        (format t "~%[IISCV-ALERT]  FALLO DE PERFORMANCE: CV ~,2f > 0.20" estabilidad)
        (format t "~%[IISCV-INFO]   PERFORMANCE ESTABLE: CV ~,2f" estabilidad))
    
    (format t "~%[IISCV-FORENSIC] Evidencia vinculada al UUID: ~A" uuid)
    (format t "~%--------------------------------------------------")))



(defun aplicar-cuarentena (registro)
  (let ((aislados '()))
    (maphash (lambda (uuid datos)
               (let ((estabilidad (getf (second datos) :ESTABILIDAD)))
                 (when (> estabilidad 1.0)
                   (format t "[ISOLATION-PROTOCOL] UUID ~A marcado para cuarentena (Estabilidad: ~F)~%" uuid estabilidad)
                   (push uuid aislados))))
             registro)
    (format t "[GLOBAL-STATUS] Total de átomos aislados: ~d~%" (length aislados))
    aislados))
