(rove/core/test:deftest :commit-8438cb0f-32c6-4b3f-82e5-c0ef1a8c7b33-test
  (rove/core/assertion:ok
   (eval '(quicklisp-client:quickload '(:hunchentoot :spinneret)))
   "The form should evaluate without error."))