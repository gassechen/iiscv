(rove/core/test:deftest :commit-cf11c54a-3717-4237-91a6-1125b3495157-test
  (rove/core/assertion:ok
   (eval
    '(defun my-divider (a b)
       "Divides two numbers and returns the result."
       (/ a b)))
   "The form should evaluate without error."))