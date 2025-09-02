(rove/core/test:deftest :commit-2f6e4b48-eb67-4685-a26c-59490d4b48b2-test
  (rove/core/assertion:ok (eval '(defclass another-class nil nil))
                          "The form should evaluate without error."))