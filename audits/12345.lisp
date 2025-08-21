(rove/core/test:deftest :commit-12345-test
  (rove/core/assertion:ok
   (eval '(defun my-test-function () "This is a test." 1))
   "The form should evaluate without error."))