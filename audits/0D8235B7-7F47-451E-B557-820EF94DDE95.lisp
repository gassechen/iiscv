(rove/core/test:deftest :commit-0d8235b7-7f47-451e-b557-820ef94dde95-test
  (rove/core/assertion:ok (eval '(defun myname (x) (+ x 3)))
                          "The form should evaluate without error."))