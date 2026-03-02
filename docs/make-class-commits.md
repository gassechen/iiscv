# Documentación de make-class-commits.lisp

## Descripción General

Proporciona macros para modificar clases existentes (añadir/remover slots) y funciones para consultar definiciones de clases desde el historial.

## Ubicación

=src/make-class-commits.lisp=

## Paquete

=iiscv=

## Dependencias

- =main.lisp= (funciones base)
- =utility-fn.lisp= (búsqueda)

## Estado

Completamente documentado

---

## Funciones de Consulta

### get-class-source-form (class-name)

**Propósito:** Recupera la forma DEFCLASS para una clase dada desde el historial atómico.

**Parámetros:**
- =class-name=: Símbolo de la clase

**Retorna:**
- Forma DEFCLASS si se encuentra
- NIL si no existe en el historial

**Ejemplo:**
#+begin_src lisp
(get-class-source-form 'mi-clase)
;; => (DEFCLASS MI-CLASE ()
;;     ((SLOT1 :INITARG :SLOT1)
;;      (SLOT2 :INITARG :SLOT2)))
#+end_src

---

### find-slot-in-form (slot-name form)

**Propósito:** Encuentra y retorna una definición de slot desde una forma DEFCLASS.

**Parámetros:**
- =slot-name=: Símbolo del slot
- =form=: Forma DEFCLASS

**Retorna:**
- Definición del slot si se encuentra
- NIL si no existe

---

### filter-slots-from-form (slot-name form)

**Propósito:** Remueve una definición de slot desde una forma DEFCLASS.

**Parámetros:**
- =slot-name=: Símbolo del slot a remover
- =form=: Forma DEFCLASS

**Retorna:**
- Nueva forma DEFCLASS sin el slot
- NIL si no es una forma DEFCLASS

---

## Macros de Modificación

### add-slot (class-name slot-definition)

**Propósito:** Añade un nuevo slot a una clase existente y committea el cambio.

**Proceso:**
1. Recupera la forma actual de la clase
2. Añade el nuevo slot a la lista de slots
3. Genera nueva forma DEFCLASS
4. La evalúa
5. Retorna la nueva forma

**Parámetros:**
- =class-name=: Símbolo de la clase
- =slot-definition=: Forma del slot (ej: =(slot-name :initarg :slot-name :accessor slot-name)=)

**Ejemplo:**
#+begin_src lisp
(add-slot 'persona '(edad :initarg :edad :accessor persona-edad :initform 0))
;; Crea y evalúa:
;; (DEFCLASS PERSONA ()
;;   ((NOMBRE :INITARG :NOMBRE :ACCESSOR PERSONA-NOMBRE)
;;    (EDAD :INITARG :EDAD :ACCESSOR PERSONA-EDAD :INITFORM 0)))
#+end_src

---

### remove-slot (class-name slot-name)

**Propósito:** Remueve un slot de una clase existente y committea el cambio.

**Proceso:**
1. Recupera la forma actual de la clase
2. Filtra el slot a remover
3. Genera nueva forma DEFCLASS
4. La evalúa
5. Retorna la nueva forma

**Parámetros:**
- =class-name=: Símbolo de la clase
- =slot-name=: Símbolo del slot a remover

**Ejemplo:**
#+begin_src lisp
(remove-slot 'persona 'edad)
;; Crea y evalúa:
;; (DEFCLASS PERSONA ()
;;   ((NOMBRE :INITARG :NOMBRE :ACCESSOR PERSONA-NOMBRE)))
;; (remueve el slot EDAD)
#+end_src

---

## Registro de Tipos

Estos macros están registrados en =register-commit-type.lisp=:

| Forma | Tipo |
|-------|------|
| =add-slot= | slot-change |
| =remove-slot= | slot-change |

---

## Flujo de Trabajo

#+begin
;; Primero, definir la clase
(make-assert '(defclass persona ()
               ((nombre :initarg :nombre))))

;; Añadir un slot
(add-slot 'persona '(edad :initarg :edad))

;; Remover un slot
(remove-slot 'persona 'edad)
#+end

---

## Integración con Otros Módulos

- =register-commit-type.lisp=: Registra =add-slot= y =remove-slot=
- =main.lisp=: Usa =get-commit-type= para categorizar
- =utility-fn.lisp=: Proporciona =get-source-form-by-uuid=

---

## Ejemplo de Uso

#+begin_src lisp
;; Consultar forma actual de una clase
(get-class-source-form 'mi-clase)

;; Añadir nuevo slot
(add-slot 'usuario '(email :initarg :email :accessor usuario-email))

;; Remover slot
(remove-slot 'usuario 'password)
#+end_src

---

## Notas

- Las modificaciones de slots se commitean como =slot-change=
- El sistema mantiene el historial de cambios de clases
- Las modificaciones son destructivas (reemplazan la definición anterior)
- Útil para evolución incremental de clases
- Requiere que la clase ya exista en el historial
