(rove/core/test:deftest :commit-262d19bd-8b27-4b23-98ad-b52ee1cd2ad7-test
  (rove/core/assertion:ok
   (eval
    '(defun test-function ()
       "This is a test function to be audited."
       (format t "Hello from a loaded file!")))
   "The form should evaluate without error."))