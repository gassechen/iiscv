(rove/core/test:deftest :commit-2ad026aa-87f3-45fe-848a-6608205cae01-test
  (rove/core/assertion:ok
   (eval
    '(defun my-first-function (x)
       "This is a simple function to demonstrate the system."
       (* x x)))
   "The form should evaluate without error."))