(rove/core/test:deftest :commit-c3aee79e-6125-4601-bc6d-fe006513ef95-test
  (rove/core/assertion:ok
   (eval
    '(defvar *todo-server*
       nil
       "The Hunchentoot server instance for the todo app."))
   "The form should evaluate without error."))