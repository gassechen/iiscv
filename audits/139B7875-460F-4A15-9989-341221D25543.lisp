(rove/core/test:deftest :commit-139b7875-460f-4a15-9989-341221d25543-test
  (rove/core/assertion:ok
   (eval
    '(defun get-todos ()
       "Returns the current list of todo items."
       (reverse *todo-list*)))
   "The form should evaluate without error."))