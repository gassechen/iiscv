(rove/core/test:deftest :commit-3078d22f-5055-4a94-9ab8-34ec58136478-test
  (rove/core/assertion:ok (eval '(defvar *new-car* (make-instance 'vehicle)))
                          "The form should evaluate without error."))