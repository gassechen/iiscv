(rove/core/test:deftest :commit-8df24bdb-e4cc-4914-ace4-a0819fa6ce3a-test
  (rove/core/assertion:ok
   (eval
    '(defstruct todo-item
       (id (uuid:make-v4-uuid) :read-only t)
       (text "" :type string)
       (done-p nil :type boolean)))
   "The form should evaluate without error."))