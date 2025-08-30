(rove/core/test:deftest :commit-e5c0c3a6-7e5e-4e7b-ae9b-41b2dc3738d5-test
  (rove/core/assertion:ok
   (eval
    '(defun my-first-function (x)
       "This is a simple function to demonstrate the system."
       (* x x)))
   "The form should evaluate without error."))