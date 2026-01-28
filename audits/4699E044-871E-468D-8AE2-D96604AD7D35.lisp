(rove/core/test:deftest :commit-4699e044-871e-468d-8ae2-d96604ad7d35-test
  (rove/core/assertion:ok
   (eval
    '(defun prueba-funcion ()
       "Función de prueba para verificar el commit"
       (+ 1 2 3)))
   "The form should evaluate without error."))