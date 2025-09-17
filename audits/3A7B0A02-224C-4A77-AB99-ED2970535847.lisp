(rove/core/test:deftest :commit-3a7b0a02-224c-4a77-ab99-ed2970535847-test
  (rove/core/assertion:ok
   (eval
    '(defun get-todos ()
       "Returns the current list of todo items."
       (reverse *todo-list*)))
   "The form should evaluate without error."))