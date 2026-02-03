;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8. IMAGE MANAGEMENT (RESTORED)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun has-pending-changes-p ()
  "Devuelve T si hay commits atómicos sin commit humano asociado.
   Maneja correctamente tanto vértices completos como UUIDs sueltos."
  (handler-case
      (progn
        (unless *atomic-history-graph*
          (return-from has-pending-changes-p nil))
        
        (let* ((last-human-time (get-timestamp-of-last-human-commit))
               (all-atomic-commits (cl-graph:vertexes *atomic-history-graph*)))
          (loop for vertex in all-atomic-commits
                for data = (cl-graph:element vertex)
                ; Ignorar elementos que no son commits válidos
                when (and (listp data)
                         (getf data :uuid)      ; Verificar que tenga UUID
                         (getf data :timestamp) ; Verificar que tenga timestamp
                         (> (getf data :timestamp) last-human-time))
                  return t)))
    (error (e)
      (format t "Error en has-pending-changes-p: ~A~%" e)
      nil)))




(defun save-development-image (path)
  "Saves a development image with all history and audit data.
   This image is useful for continuing development from the current state."
  (when (has-pending-changes-p)
    (format t "~%ERROR: Cannot save development image. There are pending atomic changes.~%")
    (format t "Please run (make-human-commit \"Descriptive message\") to consolidate changes before continuing.~%")
    (return-from save-development-image nil))
  
  (format t "~%Saving full development image...~%")
  #+sbcl (sb-ext:save-lisp-and-die path :executable t)
  #+ccl (ccl:save-application path :prepend-kernel t)
  #+abcl (ext:save-application path :executable t)
  #+ecl (ext:save-executable path :toplevel #'iiscv-repl))

;;; agregar el entry point
(defun save-production-image (path)
  "Creates a lightweight production image by rebuilding the system from human commits.
   This approach is non-destructive and more robust."
  (when (has-pending-changes-p)
    (format t "~%ERROR: Cannot create production image. There are pending atomic changes.~%")
    (format t "Please run (make-human-commit \"Descriptive message\") to consolidate changes before continuing.~%")
    (return-from save-production-image nil))
  
  (format t "~%Creating lightweight production image...~%")
  
  ;; 1. Rebuild the system from the human-curated history
  (rebuild-image-from-human-history)

  ;; 2. Once rebuilt, save the new, clean image. The history graphs are not
  ;;    included in this new image because they are not part of the `rebuild`.
  (format t "~%Reconstruction complete. Saving production image to ~a...~%" path)
  #+sbcl (sb-ext:save-lisp-and-die path :executable t)
  #+ccl (ccl:save-application path :prepend-kernel t)
  #+abcl (ext:save-application path :executable t)
  #+ecl (ext:save-executable path :toplevel #'iiscv-repl))


(defun rebuild-image-from-human-history ()
  "Rebuilds the system image by evaluating the atomic commits linked to the human history."
  (unless *human-history-graph*
    (format t "Error: The human history graph is empty. There is nothing to rebuild.~%")
    (return-from rebuild-image-from-human-history nil))
  (format t "Rebuilding image from human (curated) history...~%")
  
  (let ((human-commits (cl-graph:topological-sort *human-history-graph*)))
    (dolist (human-commit-node human-commits)
      (let* ((commit-data (cl-graph:element human-commit-node))
             (atomic-uuids (getf commit-data :atomic-uuids)))
        (format t "Processing human commit: ~A~%" (getf commit-data :message))
        (dolist (atomic-uuid atomic-uuids)
          (let* ((atomic-vertex (find-vertex-by-uuid *atomic-history-graph* atomic-uuid))
                 (atomic-data (when atomic-vertex (cl-graph:element atomic-vertex))))
            (when atomic-data
              (let ((form (getf atomic-data :source-form)))
                (format t "  -> Evaluating atomic commit ~A: ~A~%" atomic-uuid form)
                (handler-case
                    (eval form)
                  (error (e)
                    (format t "~%Error evaluating atomic commit ~A: ~A~%" atomic-uuid e))))))))))
  (format t "Reconstruction of image completed.~%"))


(defun rebuild-image-from-atomic-history ()
  "Rebuilds the system image by evaluating every atomic commit in the history. Useful for disaster recovery."
  (unless *atomic-history-graph*
    (format t "Error: The atomic history graph is empty. There is nothing to rebuild.~%")
    (return-from rebuild-image-from-atomic-history nil))
  (format t "Rebuilding image from atomic (complete) history...~%")
  
  (let ((vertices (cl-graph:topological-sort *atomic-history-graph*)))
    (dolist (vertex vertices)
      (let* ((commit-data (cl-graph:element vertex))
             (form (getf commit-data :source-form)))
        (format t "Evaluating commit ~A: ~A~%" (getf commit-data :uuid) form)
        (handler-case
            (eval form)
          (error (e)
            (format t "~%Error evaluating commit ~A: ~A~%" (getf commit-data :uuid) e)))))
    (format t "Reconstruction of image completed.~%")))


