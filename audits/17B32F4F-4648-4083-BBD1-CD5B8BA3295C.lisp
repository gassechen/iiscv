(rove/core/test:deftest :commit-17b32f4f-4648-4083-bbd1-cd5b8ba3295c-test
  (rove/core/assertion:ok (eval '(defun myname (x y) (* x y) (+ x y)))
                          "The form should evaluate without error."))