(rove/core/test:deftest :commit-93f66696-8b48-464c-a940-ffee2878bef3-test
  (rove/core/assertion:ok
   (eval
    '(defun stop-todo-server ()
       "Stops the Hunchentoot server."
       (when *todo-server* (stop *todo-server*) (setf *todo-server* nil))))
   "The form should evaluate without error."))