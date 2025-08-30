(rove/core/test:deftest :commit-07efb5bf-49f0-4e17-a157-a9f7d0f68053-test
  (rove/core/assertion:ok
   (eval
    '(defclass person nil
               ((name :accessor name :initarg :name :initform "John Doe")
                (age :accessor age :initarg :age :initform 0))))
   "The form should evaluate without error."))