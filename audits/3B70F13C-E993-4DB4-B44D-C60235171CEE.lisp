(rove/core/test:deftest :commit-3b70f13c-e993-4db4-b44d-c60235171cee-test
  (rove/core/assertion:ok
   (eval
    '(defvar *todo-server*
       nil
       "The Hunchentoot server instance for the todo app."))
   "The form should evaluate without error."))