(rove/core/test:deftest :commit-a710fdf8-88ae-4ad8-bdef-8189b6213987-test
  (rove/core/assertion:ok (eval '(defclass another-class nil nil))
                          "The form should evaluate without error."))