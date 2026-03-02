# Documentación de utility-fn.lisp

## Descripción General

Contiene funciones utilitarias para búsqueda, consulta y manipulación del historial de commits. Proporciona la API de acceso a los datos almacenados en los grafos.

## Ubicación

=src/utility-fn.lisp=

## Paquete

=iiscv=

## Estado

Completamente documentado

---

## Funciones de Consulta

### get-source-form (function-name &key full-commit-p)

**Propósito:** Recupera la forma fuente de una función desde el historial.

**Parámetros:**
- =function-name=: String con el nombre (ej: "IISCV::SUM")
- =full-commit-p=: Si T, retorna el nodo completo del commit

**Retorna:**
- Forma fuente (list) si se encuentra
- Nodo completo si =full-commit-p= es T
- NIL si no se encuentra

**Ejemplo:**
#+begin_src lisp
(get-source-form "IISCV::FOO")
;; => (DEFUN FOO (X) (+ X 1))

(get-source-form "IISCV::FOO" :full-commit-p t)
;; => (:UUID "..." :SYMBOL-NAME FOO :SOURCE-FORM ... :STATUS :EXPERIMENTAL ...)
#+end_src

---

### get-source-form-by-uuid (uuid)

**Propósito:** Recupera la forma fuente de un commit por su UUID.

**Parámetros:**
- =uuid=: Identificador único del commit

**Retorna:** Forma fuente o NIL

---

### get-last-uuid-by-name (name-symbol)

**Propósito:** Retorna el UUID activo más reciente para un símbolo dado.

**Parámetros:**
- =name-symbol=: Símbolo o string

**Retorna:** UUID como string o NIL

**Ejemplo:**
#+begin_src lisp
(get-last-uuid-by-name 'foo)
;; => "550e8400-e29b-41d4-a716-446655440000"
#+end_src

---

## Funciones de Búsqueda en Grafos

### find-vertex-by-uuid (graph uuid)

**Propósito:** Encuentra y retorna un vértice en un grafo por su propiedad UUID.

**Parámetros:**
- =graph=: Grafo de cl-graph
- =uuid=: UUID a buscar

**Retorna:** Vértice o NIL

**Algoritmo:**
1. Itera sobre todos los vértices del grafo
2. Compara la propiedad =:UUID= de cada elemento
3. Retorna el primero que coincida

---

### find-vertex-by-symbol-name (function-name)

**Propósito:** Encuentra el vértice de la versión activa de una función en el grafo atómico.

**Parámetros:**
- =function-name=: Símbolo o string

**Retorna:** Vértice o NIL

**Ejemplo:**
#+begin_src lisp
(find-vertex-by-symbol-name 'mi-funcion)
;; => #<VERTEX ...>
#+end_src

---

### get-data-from-vertex (vertex)

**Propósito:** Recupera de forma segura la lista de datos de un vértice de cl-graph.

**Parámetros:**
- =vertex=: Vértice de cl-graph

**Retorna:** Lista de propiedades o NIL

---

## Funciones de String

### string-join (list-of-strings separator)

**Propósito:** Une una lista de strings con un separador.

**Parámetros:**
- =list-of-strings=: Lista de strings
- =separator=: String separador

**Ejemplo:**
#+begin_src lisp
(string-join '("a" "b" "c") ", ")
;; => "a, b, c"
#+end_src

---

## Mapa de Funciones a UUIDs

El hash table =*function-to-uuid-map*= es la tabla principal de búsqueda:

| Clave | Valor |
|-------|-------|
| "PACKAGE::NAME" | UUID string |

**Ejemplo de estructura interna:**
#+begin_src lisp
;; Después de commitear (defun foo () ...)
(setf (gethash "IISCV::FOO" *function-to-uuid-map*) 
      "550e8400-e29b-41d4-a716-446655440000")
#+end_src

---

## Integración con Otros Módulos

- =main.lisp=: Usa =get-last-uuid-by-name= para tracking
- =make-human-commits.lisp:= Usa =find-vertex-by-uuid= y =get-last-uuid-by-name=
- =dump-source-code.lisp=: Usa =find-vertex-by-uuid=
- =reports.lisp=: Usa =get-source-form= para sanitize

---

## Ejemplo de Uso

#+begin_src lisp
;; Consultar código de una función
(get-source-form "IISCV::MI-FUNCION")

;; Obtener UUID de la última versión
(get-last-uuid-by-name 'mi-funcion)

;; Buscar vértice para análisis
(find-vertex-by-symbol-name 'mi-funcion)

;; Recuperar datos completos
(let ((vertex (find-vertex-by-symbol-name 'mi-funcion)))
  (when vertex
    (get-data-from-vertex vertex)))
#+end_src
