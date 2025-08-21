(rove/core/test:deftest :commit-ae8362ab-fd90-412e-9dd1-25f048c0aa9c-test
  (rove/core/assertion:ok
   (eval
    '(defun my-divider (a b)
       "Divides two numbers and returns the result."
       (/ a b)))
   "The form should evaluate without error."))