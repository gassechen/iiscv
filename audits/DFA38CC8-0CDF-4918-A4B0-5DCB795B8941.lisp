(rove/core/test:deftest :commit-dfa38cc8-0cdf-4918-a4b0-5dcb795b8941-test
  (rove/core/assertion:ok
   (eval
    '(defstruct todo-item
       (id (uuid:make-v4-uuid) :read-only t)
       (text "" :type string)
       (done-p nil :type boolean)))
   "The form should evaluate without error."))