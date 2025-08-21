(rove/core/test:deftest :commit-120357f7-f777-4f95-9150-34c5a075c7e8-test
  (rove/core/assertion:ok
   (eval
    '(defun my-test-function (x)
       "A simple function for testing the commit system."
       (+ x 1)))
   "The form should evaluate without error."))