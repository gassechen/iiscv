;;; main.lisp
;;; Core Orchestrator for IISCV (Immutable and Curated Version Control System)

(uiop:define-package iiscv
  (:use #:cl #:LISA-LISP)
  (:shadowing-import-from #:LISA-LISP #:assert)
  (:export #:human-commit
           #:iiscv-repl
           #:*atomic-history-graph*
           #:*human-history-graph*
           #:make-atomic-commit
           #:analyze-commit-and-assert
           #:get-last-uuid-by-name
           #:get-source-form-by-uuid
           #:rebuild-image-from-human-history
           #:rebuild-image-from-atomic-history
           #:show-atomic-commit
           #:show-human-commit
           #:show-project-milestones
           #:audit-atomic-history
           #:run-all-audits
           #:clear-all-commits
           #:manual-human-commit
           #:dump-source-code
           #:dump-source-code-by-commit-type
           #:add-slot
           #:remove-slot))

(in-package #:iiscv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. GLOBAL STATE & INITIALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *function-to-uuid-map* (make-hash-table :test 'equal)
  "Maps function names (PACKAGE::NAME) to their last committed UUID.")

(defvar *atomic-history-graph* (make-instance 'cl-graph:dot-graph)
  "Graph to store all individual, atomic commits. The machine history.")

(defvar *human-history-graph* (make-instance 'cl-graph:dot-graph)
  "Graph to store high-level milestones. The history for humans.")

(defvar *last-atomic-commit-uuid* nil
  "Reference to the UUID of the last atomic commit for chronological linking.")

(defvar *current-human-commit* nil
  "Reference to the UUID of the last human milestone.")

(defun clear-all-commits ()
  "Resets all IISCV graphs and registries."
  (setf *atomic-history-graph* (make-instance 'cl-graph:dot-graph))
  (setf *human-history-graph* (make-instance 'cl-graph:dot-graph))
  (setf *function-to-uuid-map* (make-hash-table :test 'equal))
  (setf *current-human-commit* nil)
  (setf *last-atomic-commit-uuid* nil)
  (format t "~%[IISCV] All forensic history cleared.~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. ATOMIC COMMIT SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-atomic-commit (definition-form)
  "Captures a definition, audits it via LISA, and registers it as :EXPERIMENTAL."
  (setq *audit-violations* nil)
  (lisa:reset)
  (let* ((name-form (and (listp definition-form) (second definition-form)))
         (name (if (symbolp name-form)
                   name-form
                   (intern (string-join
                            (mapcar #'princ-to-string (alexandria:flatten name-form))
                            "-"))))
         (fqn (format nil "~A::~A" (package-name (symbol-package name)) (symbol-name name)))
         (is-redef (not (null (gethash fqn *function-to-uuid-map*))))
         
         
         (calls (extract-calls definition-form name)) 
         (docstring (get-docstring definition-form))
         (body (get-body-forms definition-form))
         (mutations (extract-mutated-symbols definition-form))
         (last-val (car (last body)))
         
         (commit-uuid (format nil "~a" (uuid:make-v4-uuid)))
         (style-critiques (clean-critic-report 
                           (with-output-to-string (*standard-output*)
                             (lisp-critic:critique-definition definition-form)))))

    ;; Forensic Audit with NASA and Logic Parameters
    (analyze-commit-and-assert
     :uuid commit-uuid
     :name name
     :has-docstring-p (not (null docstring))
     :body-length (calculate-body-length definition-form)
     :cyclomatic-complexity (calculate-cyclomatic-complexity definition-form)
     :magic-numbers (find-magic-numbers definition-form)
     :unused-parameters (find-unused-parameters definition-form)
     :uses-unsafe-execution-p (not (null (find-unsafe-execution-forms definition-form)))
     :contains-heavy-consing-loop-p (contains-heavy-consing-loop-p definition-form)
     :uses-implementation-specific-symbols-p (find-implementation-specific-symbols definition-form)
     :style-critiques style-critiques
     :is-redef is-redef
     :calls calls
     :status :experimental
     :is-predicate (is-lisp-predicate-p name)
     :has-dead-code (has-dead-code-p definition-form)
     :is-recursive (is-recursive-p name definition-form)
     :assertion-count (count-assertions definition-form)
     :has-unbounded-loop (has-unbounded-loops-p definition-form)
     :has-side-effects (not (null mutations))
     :returns-constant-nil (or (null last-val) (eq last-val nil)))

    ;; Persist in Atomic Graph
    (let* ((commit-data `(:UUID ,commit-uuid
                         :symbol-name ,name
                         :source-form ,definition-form
                         :status :experimental
                         :calls ,calls
                         :message ,(or docstring "No docstring provided.")
                         :timestamp ,(get-universal-time)
                         :rules-violations ,*audit-violations*))
           (new-v (cl-graph:add-vertex *atomic-history-graph* commit-data)))
      
      (when *last-atomic-commit-uuid*
        (let ((old-v (find-vertex-by-uuid *atomic-history-graph* *last-atomic-commit-uuid*)))
          (when (and old-v new-v)
            (cl-graph:add-edge-between-vertexes *atomic-history-graph* old-v new-v))))
  
      (setf *last-atomic-commit-uuid* commit-uuid)
      (when (symbolp name)
        (setf (gethash fqn *function-to-uuid-map*) commit-uuid))

      (format t "~%[AUDIT] ~A | Violations: ~A~%" name (length *audit-violations*))
      (format t "~{~a~%~}" (mapcar #'car *audit-violations*))
      commit-uuid)))










