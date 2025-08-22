(rove/core/test:deftest :commit-6464b894-962b-47c0-8e31-a6f75d33280d-test
  (rove/core/assertion:ok (eval '(quicklisp-client:quickload :cl-who))
                          "The form should evaluate without error."))