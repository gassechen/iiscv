(rove/core/test:deftest :commit-c09f32b0-e871-423c-b911-669bf64a06e2-test
  (rove/core/assertion:ok (eval '(defun myname (x) (+ x 3)))
                          "The form should evaluate without error."))