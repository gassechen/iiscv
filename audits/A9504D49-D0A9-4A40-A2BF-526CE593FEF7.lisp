(rove/core/test:deftest :commit-a9504d49-d0a9-4a40-a2bf-526ce593fef7-test
  (rove/core/assertion:ok
   (eval
    '(defun stop-todo-server ()
       "Stops the Hunchentoot server."
       (when *todo-server*
         (hunchentoot:stop *todo-server*)
         (setf *todo-server* nil))))
   "The form should evaluate without error."))