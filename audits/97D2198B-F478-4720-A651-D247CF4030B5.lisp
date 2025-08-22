(rove/core/test:deftest :commit-97d2198b-f478-4720-a651-d247cf4030b5-test
  (rove/core/assertion:ok
   (eval '(defun add-two (x) "Adds 2 to a number." (+ x 2)))
   "The form should evaluate without error."))