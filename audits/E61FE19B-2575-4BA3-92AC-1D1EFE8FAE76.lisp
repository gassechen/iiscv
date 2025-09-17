(rove/core/test:deftest :commit-e61fe19b-2575-4ba3-92ac-1d1efe8fae76-test
  (rove/core/assertion:ok
   (eval '(quicklisp-client:quickload '(:hunchentoot :spinneret)))
   "The form should evaluate without error."))