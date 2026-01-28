(rove/core/test:deftest :commit-8adad175-97b6-4bae-8fe0-5bd3f0ddd30b-test
  (rove/core/assertion:ok
   (eval
    '(defun version-actual ()
       "Devuelve el string que identifica la versión actual del sistema IOE."
       "IOE-CON-AUTOPERSISTENCIA"))
   "The form should evaluate without error."))