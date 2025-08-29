(rove/core/test:deftest :commit-01f3afe9-8472-4595-abe4-a69e239d75ea-test
  (rove/core/assertion:ok
   (eval
    '(defclass vehicle nil
               ((make :accessor make :initarg :make :initform "Unknown")
                (model :accessor model :initarg :model :initform "Unknown")
                (color :accessor color :initarg :color :initform
                       "Not specified"))))
   "The form should evaluate without error."))