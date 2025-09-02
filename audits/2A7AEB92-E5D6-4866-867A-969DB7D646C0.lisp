(rove/core/test:deftest :commit-2a7aeb92-e5d6-4866-867a-969db7d646c0-test
  (rove/core/assertion:ok (eval '(defvar *test-variable* 100))
                          "The form should evaluate without error."))