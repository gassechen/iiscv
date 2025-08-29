(rove/core/test:deftest :commit-cf0ef852-fcf0-49e8-9054-51e8ccbcd0ff-test
  (rove/core/assertion:ok (eval '(defvar *my-car* (make-instance 'vehicle)))
                          "The form should evaluate without error."))