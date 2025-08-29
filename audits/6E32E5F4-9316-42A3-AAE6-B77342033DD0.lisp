(rove/core/test:deftest :commit-6e32e5f4-9316-42a3-aae6-b77342033dd0-test
  (rove/core/assertion:ok
   (eval
    '(defun my-second-function (x y)
       "This function adds two numbers."
       (+ x y)))
   "The form should evaluate without error."))