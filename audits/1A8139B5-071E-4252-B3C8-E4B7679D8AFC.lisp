(rove/core/test:deftest :commit-1a8139b5-071e-4252-b3c8-e4b7679d8afc-test
  (rove/core/assertion:ok
   (eval
    '(defun get-todos ()
       "Returns the current list of todo items."
       (reverse *todo-list*)))
   "The form should evaluate without error."))