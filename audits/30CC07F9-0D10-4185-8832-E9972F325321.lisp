(rove/core/test:deftest :commit-30cc07f9-0d10-4185-8832-e9972f325321-test
  (rove/core/assertion:ok
   (eval
    '(defun launch-program (program-name)
       "Launches an external program using a SBCL-specific symbol."
       (sb-ext:run-program program-name nil)))
   "The form should evaluate without error."))