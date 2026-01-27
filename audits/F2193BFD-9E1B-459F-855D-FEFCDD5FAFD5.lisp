(rove/core/test:deftest :commit-f2193bfd-9e1b-459f-855d-fefcdd5fafd5-test
  (rove/core/assertion:ok
   (eval
    '(defun chequear-presion (valor)
       (if (> valor 150)
           (format t "PELIGRO~%")
           (format t "Normal~%"))))
   "The form should evaluate without error."))