(rove/core/test:deftest :commit-38c916cb-9ed0-43f0-a44e-9ffb7a916675-test
  (rove/core/assertion:ok (eval '(in-package :ioe-app))
                          "The form should evaluate without error."))