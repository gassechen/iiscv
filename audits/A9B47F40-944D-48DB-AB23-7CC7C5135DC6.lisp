(rove/core/test:deftest :commit-a9b47f40-944d-48db-ab23-7cc7c5135dc6-test
  (rove/core/assertion:ok (eval '(load "test-file.lisp"))
                          "The form should evaluate without error."))