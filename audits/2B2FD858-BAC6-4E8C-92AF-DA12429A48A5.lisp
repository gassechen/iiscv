(rove/core/test:deftest :commit-2b2fd858-bac6-4e8c-92af-da12429a48a5-test
  (rove/core/assertion:ok
   (eval
    '(defun start-todo-server ()
       "Starts the Hunchentoot server for the todo app."
       (unless *todo-server*
         (setf *todo-server*
                 (start
                  (make-instance 'easy-acceptor :port +todo-server-port+))))))
   "The form should evaluate without error."))