(rove/core/test:deftest :commit-1f756d05-5a5b-4746-a688-ada0879ae9d6-test
  (rove/core/assertion:ok
   (eval
    '(defun my-first-function (x)
       "This is the updated version of the first function."
       (1+ (* x x))))
   "The form should evaluate without error."))