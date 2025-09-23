(rove/core/test:deftest :commit-b31724a0-1f53-43dd-9432-a3fda90fd295-test
  (rove/core/assertion:ok
   (eval
    '(defun delete-todo (id)
       "Deletes a todo item from the global list based on its ID."
       (setf *todo-list*
               (delete id *todo-list* :key #'todo-item-id :test
                       #'uuid:uuid=))))
   "The form should evaluate without error."))