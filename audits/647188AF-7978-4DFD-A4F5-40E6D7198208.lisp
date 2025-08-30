(rove/core/test:deftest :commit-647188af-7978-4dfd-a4f5-40e6d7198208-test
  (rove/core/assertion:ok
   (eval
    '(defun my-first-function (x)
       "This is a simple function to demonstrate the system."
       (* x x)))
   "The form should evaluate without error."))