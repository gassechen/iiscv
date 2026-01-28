(rove/core/test:deftest :commit-99602ac6-d92b-4b71-8dad-22357249c153-test
  (rove/core/assertion:ok
   (eval '(defconstant +uno+ 1 "Constante para el número uno"))
   "The form should evaluate without error."))