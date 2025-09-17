(rove/core/test:deftest :commit-fdb0175e-327a-4cd8-bd49-eb2e66db49a1-test
  (rove/core/assertion:ok
   (eval '(quicklisp-client:quickload '(:hunchentoot :spinneret)))
   "The form should evaluate without error."))