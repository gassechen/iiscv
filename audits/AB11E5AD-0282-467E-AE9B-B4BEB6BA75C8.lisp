(rove/core/test:deftest :commit-ab11e5ad-0282-467e-ae9b-b4beb6ba75c8-test
  (rove/core/assertion:ok
   (eval
    '(defun add-todo (text)
       "Adds a new todo item to the global todo list."
       (let ((new-item (make-todo-item :text text)))
         (push new-item *todo-list*)
         new-item)))
   "The form should evaluate without error."))