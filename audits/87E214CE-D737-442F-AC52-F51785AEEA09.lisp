(rove/core/test:deftest :commit-87e214ce-d737-442f-ac52-f51785aeea09-test
  (rove/core/assertion:ok
   (eval
    '(defun calcular-area-circulo (radio)
       "Calcula el área de un círculo dado su radio.
   Utiliza la fórmula: área = π * radio²
   Parámetro: radio - el radio del círculo (número real)
   Retorna: el área del círculo (número real)"
       (* +pi+ (* radio radio))))
   "The form should evaluate without error."))