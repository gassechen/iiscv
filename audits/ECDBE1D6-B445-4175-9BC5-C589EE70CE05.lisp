(rove/core/test:deftest :commit-ecdbe1d6-b445-4175-9bc5-c589ee70ce05-test
  (rove/core/assertion:ok
   (eval
    '(defun mark-todo-done (id)
       "Marks a todo item as done based on its ID."
       (let ((item
              (find id *todo-list* :key #'todo-item-id :test #'uuid:uuid=)))
         (when item (setf (todo-item-done-p item) t) item))))
   "The form should evaluate without error."))