(rove/core/test:deftest :commit-4c2587a1-8116-462f-a0fb-6e7d7c76b524-test
  (rove/core/assertion:ok
   (eval '(defun test-function () "Test function for commit" (+ 1 2)))
   "The form should evaluate without error."))