(rove/core/test:deftest :commit-4a1f0ef0-35ae-48ed-b4ef-9765817397e2-test
  (rove/core/assertion:ok
   (eval
    '(defun delete-todo (id)
       "Deletes a todo item from the global list based on its ID."
       (setf *todo-list*
               (delete id *todo-list* :key #'todo-item-id :test
                       #'uuid:uuid=))))
   "The form should evaluate without error."))