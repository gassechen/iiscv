(rove/core/test:deftest :commit-544753a2-d273-4fea-b5e1-27ab8c7f3daf-test
  (rove/core/assertion:ok
   (eval
    '(defun test-function ()
       "This is a test function to be audited."
       (format t "Hello from a loaded file!")))
   "The form should evaluate without error."))