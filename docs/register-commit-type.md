# Documentación de register-commit-type.lisp

## Descripción General

Define el registro central de tipos de commit. Establece qué formas Lisp (defun, defvar, etc.) son reconocidas y categorizadas por IISCV.

## Ubicación

=src/register-commit-type.lisp=

## Paquete

=iiscv=

## Estado

Completamente documentado

---

## Registro de Tipos

### Tabla de Tipos

| Form Lisp | Tipo de Commit |
|----------|---------------|
| =defun= | function |
| =defmacro= | function |
| =defvar= | variable |
| =defparameter= | variable |
| =defconstant= | variable |
| =defclass= | type |
| =defstruct= | type |
| =add-slot= | slot-change |
| =remove-slot= | slot-change |
| =ql:quickload= | dependency |

---

## Funciones del Módulo

### register-commit-type (form-name commit-type)

**Propósito:** Registra una nueva forma Lisp y su tipo de commit asociado.

**Parámetros:**
- =form-name=: Símbolo de la forma (ej: =defun=)
- =commit-type=: Tipo de commit (ej: =function=)

**Ejemplo:**
```lisp
(register-commit-type 'defgeneric 'function)
(register-commit-type 'defmethod 'function)
```

---

### get-commit-type (form)

**Propósito:** Retorna el tipo de commit para una forma de definición dada.

**Parámetros:**
- =form=: Forma Lisp (ej: =(defun foo () ...)=)

**Retorna:**
- Tipo de commit (symbol) si se encuentra
- NIL si no está registrado

**Ejemplo:**
```lisp
(get-commit-type '(defun bar (x) x))
;; => FUNCTION

(get-commit-type '(defvar x 10))
;; => VARIABLE

(get-commit-type '(print "hello"))
;; => NIL
```

---

## Hash Table de Registro

=*commit-type-registry*= es un hash table con:
- **Key:** Símbolo de forma (=defun=, =defclass=, etc.)
- **Value:** Tipo de commit (=function=, =variable=, =type=, etc.)

---

## Uso en dump-source-code

El tipo de commit se usa para agrupar definiciones en archivos separados:

```lisp
(case commit-type
  (function "functions.lisp")
  (variable "variables.lisp")
  (type "types.lisp")
  (slot-change "slot-changes.lisp")
  (dependency "dependencies.lisp")
  (t (format nil "~a.lisp" commit-type)))
```

---

## Extensión del Sistema

Para agregar nuevos tipos de commit:

1. Registrar el tipo:
```lisp
(register-commit-type 'defgeneric 'function)
(register-commit-type 'defmethod 'function)
(register-commit-type 'defpackage 'module)
```

2. Opcionalmente agregar al caso en =dump-source-code.lisp=:
```lisp
(case commit-type
  ...
  (module "modules.lisp"))
```

---

## Integración con Otros Módulos

- =main.lisp=: Usa =get-commit-type= para determinar tipo en =make-assert=
- =dump-source-code.lisp=: Agrupa por tipo usando =get-commit-type=
- =make-class-commits.lisp=: Registra =add-slot= y =remove-slot=

---

## Ejemplo de Uso

```lisp
;; Verificar tipo de una forma
(get-commit-type '(defun mi-funcion (x) x))
;; => FUNCTION

(get-commit-type '(defclass mi-clase () ((slot :initarg :slot))))
;; => TYPE

;; Registrar nuevo tipo
(register-commit-type 'defgeneric 'function)
```
