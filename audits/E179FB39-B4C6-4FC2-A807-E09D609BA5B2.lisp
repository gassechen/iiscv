(rove/core/test:deftest :commit-e179fb39-b4c6-4fc2-a807-e09d609ba5b2-test
  (rove/core/assertion:ok
   (eval
    '(defun launch-program (program-name)
       "Launches an external program using a SBCL-specific symbol."
       (sb-ext:run-program program-name nil)))
   "The form should evaluate without error."))