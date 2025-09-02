(rove/core/test:deftest :commit-5c019ad4-e192-4da0-a27e-7fb3add099f7-test
  (rove/core/assertion:ok (eval '(load "test-file.lisp"))
                          "The form should evaluate without error."))