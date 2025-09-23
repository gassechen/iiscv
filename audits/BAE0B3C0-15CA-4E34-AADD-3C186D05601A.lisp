(rove/core/test:deftest :commit-bae0b3c0-15ca-4e34-aadd-3c186d05601a-test
  (rove/core/assertion:ok
   (eval '(quicklisp-client:quickload '(:hunchentoot :spinneret)))
   "The form should evaluate without error."))