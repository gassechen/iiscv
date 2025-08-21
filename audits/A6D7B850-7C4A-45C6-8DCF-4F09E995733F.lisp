(rove/core/test:deftest :commit-a6d7b850-7c4a-45c6-8dcf-4f09e995733f-test
  (rove/core/assertion:ok
   (eval
    '(defun my-divider (a b)
       "Divides two numbers and returns the result."
       (/ a b)))
   "The form should evaluate without error."))