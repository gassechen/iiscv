(rove/core/test:deftest :commit-b9e83a7e-2bc5-44a1-899a-89e6c7c3843f-test
  (rove/core/assertion:ok
   (eval
    '(defun check-value (input-value)
       "Checks if a value is greater than a threshold."
       (let ((threshold 100))
         (when (> input-value threshold)
           (format t "Value ~A is too high.~%" input-value)
           (return-from check-value nil))
         (if (= input-value 50)
             (format t "Value is exactly 50.")
             (progn (princ "Value is not 50.") (incf input-value 1))))
       (format nil "Final value is ~A" input-value)))
   "The form should evaluate without error."))