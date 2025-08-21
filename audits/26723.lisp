(rove/core/test:deftest :commit-26723-test
  (rove/core/assertion:ok
   (eval '(progn (setf *debug-mode* 't) (setf *user-count* '200)))
   "The form should evaluate without error."))