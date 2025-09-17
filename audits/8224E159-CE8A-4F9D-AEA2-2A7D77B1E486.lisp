(rove/core/test:deftest :commit-8224e159-ce8a-4f9d-aea2-2a7d77b1e486-test
  (rove/core/assertion:ok
   (eval
    '(defun mark-todo-done (id)
       "Marks a todo item as done based on its ID."
       (let ((item
              (find id *todo-list* :key #'todo-item-id :test #'uuid:uuid=)))
         (when item (setf (todo-item-done-p item) t) item))))
   "The form should evaluate without error."))