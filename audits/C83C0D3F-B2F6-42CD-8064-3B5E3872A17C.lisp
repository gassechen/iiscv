(rove/core/test:deftest :commit-c83c0d3f-b2f6-42cd-8064-3b5e3872a17c-test
  (rove/core/assertion:ok
   (eval
    '(defun chequeo-memoria (a b)
       "Devuelve el mayor de dos números"
       (if (> a b)
           a
           b)))
   "The form should evaluate without error."))