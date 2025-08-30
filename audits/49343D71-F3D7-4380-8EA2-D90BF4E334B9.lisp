(rove/core/test:deftest :commit-49343d71-f3d7-4380-8ea2-d90bf4e334b9-test
  (rove/core/assertion:ok
   (eval
    '(defclass person nil
               ((name :accessor name :initarg :name :initform "John Doe")
                (age :accessor age :initarg :age :initform 0)
                (occupation :accessor occupation :initarg :occupation :initform
                            "Not specified"))))
   "The form should evaluate without error."))