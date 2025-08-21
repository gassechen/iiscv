(rove/core/test:deftest :commit-2347-test
  (rove/core/assertion:ok
   (eval
    '(defun my-third-audited-function ()
       "A test function to find out why the file is not being created."
       (+ 10 20)))
   "The form should evaluate without error."))