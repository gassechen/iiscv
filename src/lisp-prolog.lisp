;;; -*- Mode: LISP -*-

;;; AJY 2014-07-22 -- 2014-07-25

;;; The LISP-PROLOG from Norvig, Paradigms of Artificial Intelligence
;;; Programming
;;; Copyright (c) Peter Norvig, 1992

;;; Originally intended to convert the LM-Prolog into the CLOS
;;; This project of mine was called CLOS-Prolog
;;;
;;; The LM-Prolog system anyway turned out to be quite suboptimal from
;;; the software engineering point of view
;;; 
;;; My CLOS-Prolog project became something quite else
;;; This file is one product from the changed CLOS-Prolog project
;;;
;;; Start a Common LISP system, I recommend the Clozure Common LISP or
;;; the Allegro Common LISP, and Emacs w/ SLIME
;;; LOAD this file
;;; assert (fact) or (rule) by
;;; (<- (fact))
;;; Perform a query with 
;;; (?- (head))
;;; When the system gives alternatives for the answer,
;;; type "." or ";" as in ordinary Prolog.
;;;
;;; See the file arith-demo.lsp for an example
;;;

;;; ------------------------------------------------------------
;;; Functions from the pattern matching facility

(defpackage :prolog
  (:use :common-lisp)
  (:export :<- :?- :prove-all :get-binding :add-clause :clear-predicate 
           :clear-db :no-bindings :fail :subst-bindings
           ;; Símbolos del dominio de las reglas (Axiomas)
           :inconsistency :depends-on :is-orphan :orphan-reference 
           :arity-mismatch :expected-arity :actual-call-arity :not-equal
           :is-primitive :collect-logic-violation :clauses))

(in-package :prolog)


(defvar fail nil "Indicates pat-match failure")

(defvar no-bindings '((t . t))
  "Indicates pat-match success, w/ no variables.")

(defun variable-p (x)
  "Is x a variable?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (var . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part, for var, from binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to binding list."
  (cons (cons var val)
	(if (and (eq bindings no-bindings))
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  "Does var match input?  Uses, or updates, and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

;;; ------------------------------------------------------------
;;; Unification

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match w/ the given bindings."
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t fail)))


(defun unify-variable (var x bindings)
  "Unify var w/ x, using and possibly extending bindings."
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings))
	 fail)
	(t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur in the expression x?"
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun subst-bindings (bindings x)
  "Substitute value of variables in bindings into x,
   taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
	((eq bindings no-bindings) x)
	((and (variable-p x)
	      (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))

;;; Aux function

(proclaim '(inline reuse-cons))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or merely x-y if it is equal to (cons x y)."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


;;; ------------------------------------------------------------

;;; Clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;;; Clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

(defvar *db-predicates* nil
  "A list of all predicated in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',clause))

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    ;; (print clause)
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses for all predicates from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

;;; ------------------------------------------------------------


(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  ;; (princ "Proving goal: ") (print goal) (terpri)
  ;; (princ "Bindings    : ") (print bindings) (terpri)
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
	(some #'(lambda (clause)
		  (let ((new-clause (rename-variables clause)))
		    (prove-all
		     (append (clause-body new-clause) other-goals)
		     (unify goal (clause-head new-clause) bindings))))
	      clauses)
	;; primitive
	(funcall clauses (rest goal) bindings
		 other-goals))))

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals) bindings (rest goals)))))

;;; ------------------------------------------------------------

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree
				&optional found-so-far)
  "Return list of leaves of tree, satifying predicate,
   w/ duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate
				(rest tree)
				found-so-far))))

;;; ------------------------------------------------------------

;;; First order user interface

;;; (defmacro ?- (&rest goals) `(prove-all ',goals no-bindings))

;;; ------------------------------------------------------------

;;; Better user interface

(defmacro ?- (&rest goals)
  `(top-level-prove ',goals))


(defun top-level-prove (goals)
  "Prove goals, print variables readably."
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
	     no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable w/ its binding.
   Then ask the user for more solutions."
  (if (null vars)
      (format t "~&Yes.")
      (dolist (var vars)
	(format t "~&~a = ~a" var
		(subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

;;; ------------------------------------------------------------

;;; The consult function, AJY 2014-07-23.
;;; The (READ ...) raises a condition, END-OF-FILE, when the forms run
;;; out.
;;; The condition is caught with the (HANDLER-CASE ...) form;
;;; then the WITH-OPEN-FILE macro finishes.

(defun consult (file-name)
  (with-open-file (file-stream file-name :direction :input)
    (loop for s-exp = (handler-case
			  (read file-stream)
			  (END-OF-FILE () nil))
			  while s-exp
       do
	 (print s-exp)
	 (add-clause (list s-exp)))))

    
;;; ------------------------------------------------------------

(defun continue-p ()
  "Ask the user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t "Type ; to see more or . to stop.")
     (continue-p))))
