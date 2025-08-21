(setf *history* (make-hash-table :test 'equal))

(setf (gethash "120357F7-F777-4F95-9150-34C5A075C7E8" *history*) '(:MESSAGE
                                                                   "No docstring provided for this commit."
                                                                   :CHANGES
                                                                   ((MY-TEST-FUNCTION
                                                                     (DEFUN MY-TEST-FUNCTION
                                                                            (X)
                                                                       "A simple function for testing the commit system."
                                                                       (+ X
                                                                          1))))
                                                                   :TIMESTAMP
                                                                   3964707832
                                                                   :PARENT NIL))
(setf (gethash "851E4E89-D2AD-461F-84EA-6B1EF1A12399" *history*) '(:MESSAGE
                                                                   "El docstring está aquí."
                                                                   :CHANGES
                                                                   ((TEST-COMMIT-WITH-DOCSTRING
                                                                     (DEFUN TEST-COMMIT-WITH-DOCSTRING
                                                                            (X)
                                                                       "El docstring está aquí."
                                                                       (+ X
                                                                          2))))
                                                                   :TIMESTAMP
                                                                   3964708213
                                                                   :PARENT
                                                                   "120357F7-F777-4F95-9150-34C5A075C7E8"))
