(rove/core/test:deftest :commit-fd794630-b382-4948-9c73-5af8a0d2f2b3-test
  (rove/core/assertion:ok
   (eval
    '(defun add-todo (text)
       "Adds a new todo item to the global todo list."
       (let ((new-item (make-todo-item :text text)))
         (push new-item *todo-list*)
         new-item)))
   "The form should evaluate without error."))