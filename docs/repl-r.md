# Documentación de repl-r.lisp

## Descripción General

Proporciona un REPL interactivo personalizado para IISCV que automatiza el proceso de commit y carga de archivos con auditoría integrada.

## Ubicación

=src/repl-r.lisp=

## Paquete

=iiscv=

## Estado

Completamente documentado

---

## Funciones Principales

### iiscv-repl ()

**Propósito:** Un REPL que automáticamente hace commit de formas de definición de nivel superior y maneja errores gracefulmente.

**Características:**
- Detecta automáticamente formas que deben ser commitearas
- Commit automático antes de evaluar
- Manejo de errores sin interrumpir la sesión

**Proceso por forma:**
1. Lee la forma del input
2. Si es =load=, llama a =iiscv-load=
3. Si es una forma que debe commitearse:
   - Ejecuta =make-atomic-commit= ANTES de evaluar
4. Evalúa la forma
5. Imprime el resultado (sauf :no-print)

**Ejemplo de sesión:**
#+begin
IISCV> (defun saludar (nombre)
         (format t "Hola ~A~%" nombre))
[IISCV-OK] Átomo registrado: 550e8400-e29b-41d4-a716-446655440000
#<FUNCTION SALUDAR>

IISCV> (saludar "Mundo")
Hola Mundo
NIL
#+end

---

### iiscv-load (filename)

**Propósito:** Carga un archivo .lisp, auditando y commitendo cada definición de nivel superior.

**Proceso:**
1. Abre el archivo especificado
2. Itera sobre todas las formas
3. Para cada forma:
   - Si tiene tipo de commit registrado → =make-assert=
   - Evalúa la forma
4. Imprime confirmación al finalizar

**Parámetros:**
- =filename=: Ruta al archivo .lisp

**Ejemplo:**
#+begin_src lisp
(iiscv-load "mis-funciones.lisp")
;; [AUDIT] MI-FUNCION | Violations: 2 (0 errors, 2 warnings) | Total Score: 8
;; [IISCV-OK] Átomo registrado: ...
;; File 'mis-funciones.lisp' loaded and audited successfully.
#+end_src

---

### save-audit-vault (filename)

**Propósito:** Serializa el historial de átomos a un archivo legible.

**Proceso:**
1. Abre el archivo para escritura
2. Escribe header con timestamp
3. Imprime el =*human-history-graph*= (si soporta print-object)

**Parámetros:**
- =filename=: Ruta del archivo de vault

**Nota:** Esta función depende de que =cl-graph= soporte =print-object=.

---

## Comparación: REPL estándar vs IISCV

| Aspecto | REPL estándar | IISCV REPL |
|---------|---------------|------------|
| =defun= | Define función | Define + audit + commit |
| =defvar= | Asigna variable | Asigna + audit + commit |
| Errores | Abort | Muestra error y continúa |
| Historial | No | Sí (en grafos) |

---

## Integración con Otros Módulos

- =main.lisp=: Proporciona =make-atomic-commit= y =make-assert=
- =register-commit-type.lisp=: Detecta formas a commitear
- =make-image.lisp=: Entry point =iiscv-repl=

---

## Ejemplo de Uso

;; Iniciar REPL IISCV
(iiscv-repl)

;; Desde el REPL:
(IISCV> (defun mi-fun (x) (+ x 1))
[IISCV-OK] Átomo registrado: ...

;; Cargar archivo con auditoría
(iiscv-load "proyecto.lisp")

;; Guardar vault
(save-audit-vault "backup.vault")

---

## Notas

- El REPL usa =handler-case= para capturar errores y continuar
- Solo hace commit de formas que tienen tipo registrado (=get-commit-type=)
- El prompt muestra el paquete actual: =PACKAGE-R>=
- Útil para desarrollo interactivo con auditoría continua
- =iiscv-load= es el equivalente de =load= pero con auditoría
