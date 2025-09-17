(rove/core/test:deftest :commit-2425df8e-a7aa-4c1e-bccc-da7fb34719d9-test
  (rove/core/assertion:ok
   (eval
    '(defun stop-todo-server ()
       "Stops the Hunchentoot server."
       (when *todo-server* (stop *todo-server*) (setf *todo-server* nil))))
   "The form should evaluate without error."))