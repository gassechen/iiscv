;; En tu archivo ~/.emacs.d/init.el

(defun my-slime-eval-and-commit ()
  "Selecciona la forma actual usando comandos nativos de Emacs y la envía a SLIME."
  (interactive)
  (save-excursion
    (let ((start (point)))
      ;; Mueve el cursor al inicio de la forma actual
      (backward-sexp)
      (setq start (point))
      ;; Mueve el cursor al final de la forma actual
      (forward-sexp)
      (let* ((end (point))
             (form-text (buffer-substring-no-properties start end))
             ;; CORRECCIÓN: Usamos (car ...) para obtener solo el objeto Lisp, no la posición
             (form (car (read-from-string form-text))))
        (if form
            (progn
              (slime-eval `(iiscv:make-atomic-commit ',form))
              (message "Committed: %S" form))
          (error "No se pudo parsear la forma Lisp. Revisa los paréntesis."))))))

;; Asegúrate de que la tecla siga asignada
(define-key slime-mode-map (kbd "C-c C-c") 'my-slime-eval-and-commit)
