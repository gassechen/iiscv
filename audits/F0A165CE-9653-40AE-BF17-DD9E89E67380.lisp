(rove/core/test:deftest :commit-f0a165ce-9653-40ae-bf17-dd9e89e67380-test
  (rove/core/assertion:ok
   (eval
    '(defun my-divider (a b)
       "Divides two numbers and returns the result."
       (/ a b)))
   "The form should evaluate without error."))