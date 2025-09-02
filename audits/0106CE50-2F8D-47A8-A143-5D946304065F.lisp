(rove/core/test:deftest :commit-0106ce50-2f8d-47a8-a143-5d946304065f-test
  (rove/core/assertion:ok (eval '(defvar *test-variable* 100))
                          "The form should evaluate without error."))