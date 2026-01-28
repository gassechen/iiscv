(rove/core/test:deftest :commit-fc96c7d5-ad93-49eb-937d-139accf457db-test
  (rove/core/assertion:ok
   (eval '(defun prueba-adn () "Identificador de memoria" :memoria-viva))
   "The form should evaluate without error."))