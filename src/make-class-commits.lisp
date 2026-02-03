;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. CLASS & SLOT MACROS (RESTORED)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iiscv)

(defun get-class-source-form (class-name)
  "Retrieves the DEFCLASS form for a given class from the atomic history."
  (let* ((uuid (gethash (format nil "~A::~A" (package-name (symbol-package class-name))
                                (symbol-name class-name))
                        *function-to-uuid-map*)))
    (when uuid
      (get-source-form-by-uuid uuid))))

(defun find-slot-in-form (slot-name form)
  "Finds and returns a slot definition from a DEFCLASS form."
  (when (and (listp form) (eq (car form) 'defclass))
    (find slot-name (nth 2 form) :key #'car)))

(defun filter-slots-from-form (slot-name form)
  "Removes a slot definition from a DEFCLASS form."
  (when (and (listp form) (eq (car form) 'defclass))
    (let ((slots (nth 2 form)))
      (remove slot-name slots :key #'car))))

(defmacro add-slot (class-name slot-definition)
  "Adds a new slot to an existing class and commits the change."
  `(let* ((current-form (get-class-source-form ,class-name)))
     (unless current-form
       (error "Class ~A not found in commit history." ,class-name))
     (let* ((new-slots (append (nth 2 current-form) (list ,slot-definition)))
            (new-form `(defclass ,(first (cdr current-form))
                           ,(second (cadr current-form))
                         ,new-slots)))
       (eval new-form)
       new-form)))

(defmacro remove-slot (class-name slot-name)
  "Removes a slot from an existing class and commits the change."
  `(let* ((current-form (get-class-source-form ,class-name)))
     (unless current-form
       (error "Class ~A not found in commit history." ,class-name))
     (let* ((new-slots (remove ,slot-name (nth 2 current-form) :key #'car))
            (new-form `(defclass ,(first (cdr current-form))
                           ,(second (cadr current-form))
                         ,new-slots)))
       (eval new-form)
       new-form)))


