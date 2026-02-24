;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11. VISUALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun show-atomic-commit () (cl-graph:vertexes *atomic-history-graph*))
(defun show-human-commit () (cl-graph:vertexes *human-history-graph*))

(defun show-project-milestones ()
  "Displays the curated project history by navigating the human commits."
  (format t "~%--- Project Milestones (Human History) ---~%")
  (let ((human-commits (cl-graph:topological-sort *human-history-graph*)))
    (dolist (commit-node human-commits)
      (let ((data (get-data-from-vertex commit-node)))
        (when data
          (format t "~%* Milestone: ~A~%" (getf data :message))
          (format t "  UUID: ~A~%" (getf data :uuid))
          (format t "  Timestamp: ~A~%" (getf data :timestamp))
          (format t "  Atomic Changes: ~A~%" (getf data :atomic-uuids))))))
  (format t "--------------------------------------------~%"))

(defun audit-atomic-history ()
  "Displays the complete and detailed history by navigating the atomic commits."
  (format t "~%--- Atomic History Audit (Blockchain) ---~%")
  (let ((atomic-commits (cl-graph:topological-sort *atomic-history-graph*)))
    (dolist (commit-node atomic-commits)
      (let ((data (get-data-from-vertex commit-node)))
        (when data
          (format t "~%* Atomic Commit: ~A~%" (getf data :uuid))
          (format t "  Message: ~A~%" (getf data :message))
          (format t "  Form: ~A~%" (getf data :source-form))
          (format t "  Timestamp: ~A~%" (getf data :timestamp))
          (format t "  Violations detected: ~A~%" (mapcar #'car (getf data :rules-violations))))))))


(defun triage-atomic-debt-report ()
  "Genera un reporte técnico de los átomos con deuda, ordenados por score de mayor a menor."
  (format t "~%==================================================================")
  (format t "~%   IISCV FORENSIC REPORT: DEUDA TÉCNICA DEL GRAFO ATÓMICO")
  (format t "~%==================================================================~%")
  (let* ((nodes (cl-graph:vertexes *atomic-history-graph*))
         (debt-list (loop for node in nodes
                          for data = (cl-graph:element node)
                          ;; El 4to elemento de cada violación es el peso numérico (score)
                          for score = (reduce #'+ (getf data :rules-violations) 
                                             :key (lambda (v) (or (fourth v) 0)) 
                                             :initial-value 0)
                          when (> score 0)
                          collect (list :name (getf data :symbol-name)
                                        :score score
                                        :violations (getf data :rules-violations)
                                        :uuid (getf data :uuid)))))
    
    ;; Ordenamiento descendente por severidad (score)
    (setf debt-list (sort debt-list #'> :key (lambda (x) (getf x :score))))

    (if (null debt-list)
        (format t "[ESTADO: IMPECABLE] No se encontró deuda técnica en los átomos registrados.~%")
        (dolist (item debt-list)
          (format t "[SCORE: ~3A] Átomo: ~A~%" (getf item :score) (getf item :name))
          (format t "             UUID: ~A~%" (getf item :uuid))
          (dolist (v (getf item :violations))
            (format t "             [-] ~A (~A pts)~%" (first v) (fourth v)))
          (format t "------------------------------------------------------------------~%")))
    (format t "~%Reporte finalizado. Usa (sanitize-atom \"~A\") para el primer objetivo.~%" 
            (if debt-list (getf (first debt-list) :name) "NONE")))
  (values))



(defun sanitize-atom (function-name)
  "Sanea una función específica recuperando su forma del grafo mediante get-source-form."
  (let ((commit-data (get-source-form function-name :full-commit-p t)))
    (if (not commit-data)
        (format t "~%[ERROR] No se encontró el átomo '~A' en el historial." function-name)
        (let ((name (getf commit-data :symbol-name))
              (form (getf commit-data :source-form))
              (violations (getf commit-data :rules-violations)))
          
          (format t "~%================================================")
          (format t "~%[AUDIT-REPAIR] Saneando átomo: ~A" name)
          (format t "~%Violaciones actuales: ~A" (mapcar #'car violations))
          (format t "~%------------------------------------------------")
          (format t "~%Código en el grafo:~%~S" form)
          (format t "~%------------------------------------------------")
          (format t "~%Pega la nueva definición para sobreescribir:")
          (finish-output)
          (clear-input)
          
          (let ((new-form (read)))
            (if (and new-form (listp new-form))
                (progn
                  (format t "~%[MURO] Re-evaluando integridad de ~A..." name)
                  (make-assert new-form)
                  (lisa:run))
                (format t "~%[CANCELADO] No se realizaron cambios.")))))))

