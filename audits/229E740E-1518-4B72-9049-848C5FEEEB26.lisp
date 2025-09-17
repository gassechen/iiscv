(rove/core/test:deftest :commit-229e740e-1518-4b72-9049-848c5feeeb26-test
  (rove/core/assertion:ok
   (eval
    '(defvar *todo-list* nil "A global list to store all todo-item objects."))
   "The form should evaluate without error."))