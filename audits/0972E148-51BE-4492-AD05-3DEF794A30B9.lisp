(rove/core/test:deftest :commit-0972e148-51be-4492-ad05-3def794a30b9-test
  (rove/core/assertion:ok
   (eval
    '(defun delete-todo (id)
       "Deletes a todo item from the global list based on its ID."
       (setf *todo-list*
               (delete id *todo-list* :key #'todo-item-id :test
                       #'uuid:uuid=))))
   "The form should evaluate without error."))