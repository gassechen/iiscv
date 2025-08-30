(rove/core/test:deftest :commit-75fd2e50-ea2e-4785-8bc6-0ff4d0f38536-test
  (rove/core/assertion:ok
   (eval
    '(defun my-first-function (x)
       "This is a simple function to demonstrate the system."
       (* x x)))
   "The form should evaluate without error."))