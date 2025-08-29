(rove/core/test:deftest :commit-fb610814-303c-42ad-89f1-24aaaee2321e-test
  (rove/core/assertion:ok
   (eval
    '(defun my-third-function (a b c)
       "This function calculates the average of three numbers."
       (/ (+ a b c) 3)))
   "The form should evaluate without error."))