(rove/core/test:deftest :commit-bd43a8d0-2d3a-47cb-8af4-b62343508503-test
  (rove/core/assertion:ok
   (eval
    '(defun todo-page ()
       "Generates the HTML for the main todo list page."
       (with-html-string
         (:doctype)
         (:html
          (:head (:title "My Lisp Todo App") (:meta :charset "utf-8")
           (:script :src "https://unpkg.com/htmx.org@1.9.10" :integrity
            "sha384-D1Kt99ot9QoT8xY1I6I6K1l6H2C8P8/n9eT3B1B8R5p4l1o+r+P2V9sW9B0O2jA"
            :crossorigin "anonymous"))
          (:body (:h1 "My Todo List")
           (:ul
            (loop for item in (get-todos)
                  do (with-html
                       (:li (todo-item-text item)))))
           (:form :hx-post "/add" :hx-target "ul" :hx-swap "beforeend"
            (:input :type "text" :name "text" :placeholder
             "Add a new todo item...")
            (:button :type "submit" "Add")))))))
   "The form should evaluate without error."))