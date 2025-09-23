(rove/core/test:deftest :commit-bf72ee6f-5d1b-41ac-be69-4c1445227712-test
  (rove/core/assertion:ok
   (eval
    '(defvar *todo-list* nil "A global list to store all todo-item objects."))
   "The form should evaluate without error."))