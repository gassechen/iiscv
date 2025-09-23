(rove/core/test:deftest :commit-3e6e81e4-1d17-4106-9bfc-be23e858c5f0-test
  (rove/core/assertion:ok
   (eval
    '(defconstant +todo-server-port+
       4442
       "The port for the Hunchentoot server."))
   "The form should evaluate without error."))