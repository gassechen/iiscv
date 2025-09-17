(rove/core/test:deftest :commit-ac00b406-3944-416d-9b00-c40b2599cd78-test
  (rove/core/assertion:ok
   (eval
    '(defconstant +todo-server-port+
       4442
       "The port for the Hunchentoot server."))
   "The form should evaluate without error."))