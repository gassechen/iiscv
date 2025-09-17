(rove/core/test:deftest :commit-7876eb35-b935-420b-96dc-e67dd1b00e17-test
  (rove/core/assertion:ok
   (eval
    '(defconstant +todo-server-port+
       4442
       "The port for the Hunchentoot server."))
   "The form should evaluate without error."))