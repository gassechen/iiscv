(rove/core/test:deftest :commit-af640c50-7aef-471f-8e67-1f27d752393a-test
  (rove/core/assertion:ok
   (eval
    '(defun add-todo (text)
       "Adds a new todo item to the global todo list."
       (let ((new-item (make-todo-item :text text)))
         (push new-item *todo-list*)
         new-item)))
   "The form should evaluate without error."))