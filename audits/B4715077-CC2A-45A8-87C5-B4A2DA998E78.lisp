(rove/core/test:deftest :commit-b4715077-cc2a-45a8-87c5-b4a2da998e78-test
  (rove/core/assertion:ok
   (eval
    '(defun start-todo-server ()
       "Starts the Hunchentoot server for the todo app."
       (unless *todo-server*
         (setf *todo-server*
                 (start
                  (make-instance 'easy-acceptor :port +todo-server-port+))))))
   "The form should evaluate without error."))