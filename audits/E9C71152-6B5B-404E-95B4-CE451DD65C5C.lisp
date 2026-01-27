(rove/core/test:deftest :commit-e9c71152-6b5b-404e-95b4-ce451dd65c5c-test
  (rove/core/assertion:ok
   (eval
    '(defun saludar (nombre)
       "Saluda a la persona cuyo nombre se proporciona como argumento.
   Esta función toma un nombre como parámetro y muestra un mensaje
   de saludo personalizado en la consola."
       (format t "¡Hola, ~A! ~%" nombre)))
   "The form should evaluate without error."))