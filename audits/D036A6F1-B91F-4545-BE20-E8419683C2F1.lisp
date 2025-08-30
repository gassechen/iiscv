(rove/core/test:deftest :commit-d036a6f1-b91f-4545-be20-e8419683c2f1-test
  (rove/core/assertion:ok
   (eval
    '(defun my-test-function (x)
       "A simple test function using a standard symbol."
       (* x x)))
   "The form should evaluate without error."))