(rove/core/test:deftest :commit-b84163ca-4ce5-4503-bff5-b24d287bac33-test
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