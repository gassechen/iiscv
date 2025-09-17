(rove/core/test:deftest :commit-9227d096-2399-40b3-9beb-ad69d60665d5-test
  (rove/core/assertion:ok
   (eval
    '(defvar *todo-server*
       nil
       "The Hunchentoot server instance for the todo app."))
   "The form should evaluate without error."))