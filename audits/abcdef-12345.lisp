(rove/core/test:deftest :|COMMIT-abcdef-12345-TEST|
  (rove/core/assertion:ok
   (eval '(defun my-sample-function () "A sample function." 42))
   "The form should evaluate without error."))