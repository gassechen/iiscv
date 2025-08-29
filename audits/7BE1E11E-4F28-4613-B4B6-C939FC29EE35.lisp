(rove/core/test:deftest :commit-7be1e11e-4f28-4613-b4b6-c939fc29ee35-test
  (rove/core/assertion:ok
   (eval
    '(defclass vehicle nil
               ((make :accessor make :initarg :make :initform "Unknown")
                (model :accessor model :initarg :model :initform "Unknown"))))
   "The form should evaluate without error."))