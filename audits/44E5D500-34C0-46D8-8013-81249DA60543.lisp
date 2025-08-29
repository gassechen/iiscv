(rove/core/test:deftest :commit-44e5d500-34c0-46d8-8013-81249da60543-test
  (rove/core/assertion:ok
   (eval
    '(defun get-docstring (definition-form)
       "Extrae el docstring de una forma de definición de función,
   manejando correctamente la posición de la lista de parámetros."
       (let ((docstring-candidate (nthcdr 2 definition-form)))
         (loop for form in docstring-candidate
               when (stringp form)
               do (return form)))))
   "The form should evaluate without error."))