;; test-file.lisp
(in-package :iiscv)

(defun test-function ()
  "This is a test function to be audited."
  (format t "Hello from a loaded file!"))

(defvar *test-variable* 100)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :todo-app)



(ql:quickload '(:hunchentoot :spinneret))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package "TODO-APP"))
    (defpackage "TODO-APP"
      (:use #:cl #:hunchentoot #:spinneret)
      (:export #:start-todo-server
               #:stop-todo-server))))


(defstruct todo-item
  (id (uuid:make-v4-uuid) :read-only t)
  (text "" :type string)
  (done-p nil :type boolean))


 (defvar *todo-list* nil
   "A global list to store all todo-item objects.")

(defun add-todo (text)
  "Adds a new todo item to the global todo list."
  (let ((new-item (make-todo-item :text text)))
    (push new-item *todo-list*)
    new-item))


(defun get-todos ()
  "Returns the current list of todo items."
  (reverse *todo-list*))


(defun mark-todo-done (id)
  "Marks a todo item as done based on its ID."
  (let ((item (find id *todo-list* :key #'todo-item-id :test #'uuid:uuid=)))
    (when item
      (setf (todo-item-done-p item) t)
      item)))


(defun delete-todo (id)
  "Deletes a todo item from the global list based on its ID."
  (setf *todo-list* (delete id *todo-list* :key #'todo-item-id :test #'uuid:uuid=)))


(defvar *todo-server* nil
  "The Hunchentoot server instance for the todo app.")

(defconstant +todo-server-port+ 4442
  "The port for the Hunchentoot server.")

(defun start-todo-server ()
  "Starts the Hunchentoot server for the todo app."
  (unless *todo-server*
    (setf *todo-server*
          (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port +todo-server-port+)))))


(defun stop-todo-server ()
  "Stops the Hunchentoot server."
  (when *todo-server*
    (hunchentoot:stop *todo-server*)
    (setf *todo-server* nil)))


(defun todo-page ()
  "Generates the HTML for the main todo list page."
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "My Lisp Todo App")
      (:meta :charset "utf-8")
      (:script :src "https://unpkg.com/htmx.org@1.9.10"
               :integrity "sha384-D1Kt99ot9QoT8xY1I6I6K1l6H2C8P8/n9eT3B1B8R5p4l1o+r+P2V9sW9B0O2jA"
               :crossorigin "anonymous"))
     (:body
      (:h1 "My Todo List")
      (:ul
       (loop for item in (get-todos) do
         (spinneret:with-html
           (:li (todo-item-text item)))))
      (:form :hx-post "/add" :hx-target "ul" :hx-swap "beforeend"
       (:input :type "text" :name "text" :placeholder "Add a new todo item...")
       (:button :type "submit" "Add"))))))


(hunchentoot:define-easy-handler (home-page :uri "/") ()
  "The main handler for the todo app's homepage."
  (todo-page))


