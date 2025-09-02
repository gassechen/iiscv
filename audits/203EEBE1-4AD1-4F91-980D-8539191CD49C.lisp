(rove/core/test:deftest :commit-203eebe1-4ad1-4f91-980d-8539191cd49c-test
  (rove/core/assertion:ok (eval '(load "test-file.lisp"))
                          "The form should evaluate without error."))