(rove/core/test:deftest :commit-e809ee24-3d27-4b46-a7c5-8d4cc8ca0fbe-test
  (rove/core/assertion:ok (eval '(defun versio-sistema () "IOE-V3"))
                          "The form should evaluate without error."))