(in-package :iiscv)

(defun benchmark-con-registro (nombre-atomo funcion args &key (iteraciones 50))
  (let* ((muestras '())
         (nombre-str (format nil "~A" nombre-atomo))
         ;; 1. Buscamos la raíz del proyecto (donde está el .asd)
         (raiz-proyecto (asdf:system-source-directory :iiscv))
         ;; 2. Definimos la carpeta de logs en la raíz, NO en src
         (dir-logs (uiop:merge-pathnames* "perf-log/" raiz-proyecto))
         (archivo (uiop:merge-pathnames* (format nil "~A-perf.log" nombre-str) dir-logs)))
    
    ;; Aseguramos que exista la carpeta en la raíz
    (ensure-directories-exist archivo)

    (with-open-file (stream archivo :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format t "~%[IISCV] Midiendo: ~A" nombre-str)
      (format t "~%[LOG] Guardando en raíz del proyecto: ~A~%" (uiop:native-namestring archivo))
      (format stream "timestamp,iteracion,delta~%")
      
      (dotimes (i iteraciones)
        (let ((t-inicio (get-internal-run-time)))
          (apply (if (symbolp funcion) (symbol-function funcion) funcion) args)
          (let ((delta (- (get-internal-run-time) t-inicio)))
            (push delta muestras)
            (format stream "~A,~D,~D~%" (get-universal-time) i delta)))))

    (let ((mediana (alexandria:median muestras))
          (desvio (alexandria:standard-deviation muestras)))
      (format t "~%--- ESTADÍSTICA ---~%")
      (format t "Mediana: ~A | Desvío: ~F~%" mediana desvio)
      (list :mediana mediana :desvio desvio :archivo (uiop:native-namestring archivo)))))



(deftemplate perf-report ()
  (slot nombre)
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
  (test (and (numberp ?est) (> ?est 0.20)))
  =>
  ;; 5. Generamos el hecho 'violation'. 
  ;; El rule-bridge-violations se encargará de hacer el PUSH y el RETRACT.
  (assert (violation (rule-id "PERF-1")
                    (severity :warning)
                    (message (format nil "Performance inestable detectada en '~a' (CV: ~,2f)." ?name ?est))
                    (score 15)))
  (format t "~%[LISA-PERF] Violación generada para ~A. El Bridge la procesará." ?name))



(defun inyectar-reporte-final (nombre-atomo stats)
  "Sigue exactamente el patrón de analyze-commit-and-assert: construye el hecho y lo evalúa."
  (let* ((med (getf stats :mediana))
         (std (getf stats :desvio))
         (cv (if (or (null med) (zerop med)) 0 (/ std med)))
         ;; Construimos el hecho según tu patrón
         (fact-data
           `(perf-report 
             (nombre ,(format nil "~A" nombre-atomo))
             (mediana ,med)
             (desvio ,std)
             (estabilidad ,cv)
             (log-path ,(getf stats :archivo)))))
    
    ;; Usamos tu método de inyección: eval + backquote
    (eval `(lisa:assert ,fact-data))
    
    ;; Ejecutamos el motor (doble run como en tu ejemplo por seguridad)
    (lisa:run)
    (format t "~%[IISCV-PERF] Hecho inyectado mediante EVAL para: ~A" nombre-atomo)))
