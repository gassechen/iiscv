(rove/core/test:deftest :commit-83a4ee6c-3c31-49ce-9521-f32c725010eb-test
  (rove/core/assertion:ok
   (eval
    '(defun open-directory (path)
       "Opens a directory using an SBCL-specific symbol."
       (sb-ext:run-program "xdg-open" (list path))))
   "The form should evaluate without error."))