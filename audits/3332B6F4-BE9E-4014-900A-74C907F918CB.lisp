(rove/core/test:deftest :commit-3332b6f4-be9e-4014-900a-74c907f918cb-test
  (rove/core/assertion:ok
   (eval
    '(defun open-directory (path)
       "Opens a directory using an SBCL-specific symbol."
       (sb-ext:run-program "xdg-open" (list path))))
   "The form should evaluate without error."))