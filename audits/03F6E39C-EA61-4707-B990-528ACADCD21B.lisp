(rove/core/test:deftest :commit-03f6e39c-ea61-4707-b990-528acadcd21b-test
  (rove/core/assertion:ok
   (eval
    '(defstruct todo-item
       (id (uuid:make-v4-uuid) :read-only t)
       (text "" :type string)
       (done-p nil :type boolean)))
   "The form should evaluate without error."))