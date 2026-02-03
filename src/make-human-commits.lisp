;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. HUMAN COMMIT SYSTEM (MILESTONES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun get-timestamp-of-last-human-commit ()
  "Returns the timestamp of the last human commit, or 0 if none exist."
  (if *current-human-commit*
      (let* ((vertex (find-vertex-by-uuid *human-history-graph* *current-human-commit*))
             (data (when vertex (cl-graph:element vertex))))
        (if (and (listp data) (getf data :timestamp))
            (getf data :timestamp)
            0))
      0))

(defun make-human-commit (message)
  "Consolidates recent atomic changes into a milestone and promotes them to :CURATED."
  (let* ((all-vertices (cl-graph:vertexes *atomic-history-graph*))
         (last-human-time (get-timestamp-of-last-human-commit))
         (recent-symbols (loop for vertex in all-vertices
                               for data = (cl-graph:element vertex)
                               when (and (listp data)           
                                         (getf data :timestamp)
                                         (> (getf data :timestamp) last-human-time))
                               collect (getf data :symbol-name)))
         (unique-symbols (remove-duplicates recent-symbols :test #'equal)))

    (if unique-symbols
        (manual-human-commit message unique-symbols)
        (format t "~%[IISCV] No pending changes to commit.~%"))))

(defun manual-human-commit (message symbols &optional (file-path nil))
  "Manually creates a milestone for specific symbols and sets their status to :CURATED."
  (let* ((atomic-uuids (loop for sym in symbols
                             for uuid = (get-last-uuid-by-name sym)
                             when uuid collect uuid))
         (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         (commit-data `(:UUID ,commit-uuid
                        :message ,message
                        :atomic-uuids ,atomic-uuids
                        :timestamp ,(get-universal-time)
                        :file-path ,file-path)))
    
    ;; 1. Promote Atomic Commits to :CURATED
    (dolist (uuid atomic-uuids)
      (let* ((vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (data (when vertex (cl-graph:element vertex))))
        (when data
          (setf (getf (cl-graph:element vertex) :status) :curated)
          (format t "[CURATION] ~A promoted to :CURATED.~%" (getf data :symbol-name)))))

    ;; 2. Human History Ledger
    (let ((new-v (cl-graph:add-vertex *human-history-graph* commit-data)))
      (when *current-human-commit*
        (let ((old-v (find-vertex-by-uuid *human-history-graph* *current-human-commit*)))
          (when (and old-v new-v)
            (cl-graph:add-edge-between-vertexes *human-history-graph* old-v new-v)))))

    (setf *current-human-commit* commit-uuid)
    
    ;; 3. Generate Output Files & Tests
    ;;(make-human-history-file (or file-path (format nil "milestone-~A.lisp" commit-uuid)) commit-data)
    ;;(generate-tests-for-human-commit commit-data)
    
    (format t "~%[IISCV] Human commit created: ~A~%" message)
    commit-uuid))


