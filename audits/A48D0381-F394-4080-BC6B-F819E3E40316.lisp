(rove/core/test:deftest :commit-a48d0381-f394-4080-bc6b-f819e3e40316-test
  (rove/core/assertion:ok (eval '(defun myname (x y) (* x y) (+ x y)))
                          "The form should evaluate without error."))