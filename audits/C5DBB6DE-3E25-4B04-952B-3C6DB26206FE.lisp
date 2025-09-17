(rove/core/test:deftest :commit-c5dbb6de-3e25-4b04-952b-3c6db26206fe-test
  (rove/core/assertion:ok
   (eval
    '(defun mark-todo-done (id)
       "Marks a todo item as done based on its ID."
       (let ((item
              (find id *todo-list* :key #'todo-item-id :test #'uuid:uuid=)))
         (when item (setf (todo-item-done-p item) t) item))))
   "The form should evaluate without error."))