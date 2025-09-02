(rove/core/test:deftest :commit-4609b93b-4876-4bed-bb10-a9befea6dc47-test
  (rove/core/assertion:ok (eval '(defvar *test-variable* 100))
                          "The form should evaluate without error."))