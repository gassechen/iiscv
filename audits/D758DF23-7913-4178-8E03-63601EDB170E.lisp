(rove/core/test:deftest :commit-d758df23-7913-4178-8e03-63601edb170e-test
  (rove/core/assertion:ok
   (eval
    '(defun add-todo (text)
       "Adds a new todo item to the global todo list."
       (let ((new-item (make-todo-item :text text)))
         (push new-item *todo-list*)
         new-item)))
   "The form should evaluate without error."))