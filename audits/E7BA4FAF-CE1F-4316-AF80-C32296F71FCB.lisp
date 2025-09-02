(rove/core/test:deftest :commit-e7ba4faf-ce1f-4316-af80-c32296f71fcb-test
  (rove/core/assertion:ok (eval '(load "test-file.lisp"))
                          "The form should evaluate without error."))