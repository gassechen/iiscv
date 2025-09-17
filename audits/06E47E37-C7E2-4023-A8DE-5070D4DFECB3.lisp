(rove/core/test:deftest :commit-06e47e37-c7e2-4023-a8de-5070d4dfecb3-test
  (rove/core/assertion:ok
   (eval
    '(defun start-todo-server ()
       "Starts the Hunchentoot server for the todo app."
       (unless *todo-server*
         (setf *todo-server*
                 (hunchentoot:start
                  (make-instance 'hunchentoot:easy-acceptor :port
                                 +todo-server-port+))))))
   "The form should evaluate without error."))