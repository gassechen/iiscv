(rove/core/test:deftest :commit-3dcd20e5-9c1f-4abb-af03-1ecdd99f7830-test
  (rove/core/assertion:ok
   (eval
    '(defun test-function ()
       "This is a test function to be audited."
       (format t "Hello from a loaded file!")))
   "The form should evaluate without error."))