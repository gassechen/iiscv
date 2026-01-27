;;;; iiscv.lisp
;; Referencia al paquete IISCV en el proyecto pi-gpio-dsl
;; Este archivo simplemente depende de que IISCV esté cargado

(in-package #:cl-user)

;; Verificar que IISCV esté disponible
(unless (find-package :iiscv)
  (error "IISCV package not found. Please load iiscv system first."))

;; Re-exportar símbolos clave de IISCV para uso en el DSL
(cl:import '(iiscv:make-atomic-commit
               iiscv:iiscv-repl
               iiscv:get-last-uuid-by-name
               iiscv:get-source-form
               iiscv:show-project-milestones
               iiscv:rebuild-image-from-human-history
               iiscv:save-production-image)
       :cl-user)
