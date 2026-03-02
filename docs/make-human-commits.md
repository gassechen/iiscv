# Documentación de make-human-commits.lisp

## Descripción General

Implementa el sistema de commits humanos (hitos/milestones). Los commits atómicos se agrupan en hitos significativos que representan puntos de control en el desarrollo, permitiendo la promoción de código de =:experimental= a =:curated=.

## Ubicación

=src/make-human-commits.lisp=

## Paquete

=iiscv=

## Dependencias

- =main.lisp= (grafos y funciones base)
- =utility-fn.lisp= (búsqueda)

## Estado

Completamente documentado

---

## Funciones Principales

### get-timestamp-of-last-human-commit ()

**Propósito:** Retorna el timestamp del último commit humano, o 0 si no existe.

**Retorna:** Timestamp universal o 0

---

### make-human-commit (message)

**Propósito:** Consolida los cambios atómicos recientes en un hito y los promociona a =:CURATED=.

**Proceso:**
1. Obtiene todos los vértices del grafo atómico
2. Filtra los símbolos con timestamp > último commit humano
3. Elimina duplicados
4. Llama a =manual-human-commit= con la lista de símbolos

**Parámetros:**
- =message=: Descripción del hito

**Retorna:** UUID del hito o NIL si no hay cambios pendientes

---

### manual-human-commit (message symbols &optional file-path)

**Propósito:** Crea manualmente un hito para símbolos específicos y establece su status como =:CURATED=.

**Proceso:**
1. Genera lista de UUIDs atómicos para los símbolos dados
2. Genera UUID único para el hito
3. **Promociona** cada commit atómico:
   - Busca el vértice por UUID
   - Cambia el =:status= de =:experimental= a =:curated=
4. Crea vértice en =*human-history-graph*= con:
   - =:UUID=: Identificador del hito
   - =:message=: Descripción
   - =:atomic-uuids=: Lista de UUIDs atómicos
   - =:timestamp=: Tiempo universal
   - =:file-path=: Ruta opcional
5. Crea arista desde el hito anterior
6. Actualiza =*current-human-commit*=

**Parámetros:**
- =message=: Descripción del hito
- =symbols=: Lista de símbolos a incluir
- =file-path=: Ruta opcional del archivo

**Retorna:** UUID del hito creado

---

## Sistema de Curación (Curation)

El sistema de curación es的核心 de la política de calidad:

### Estados de Código

| Estado | Descripción |
|--------|-------------|
| =:experimental= | Código nuevo, en auditoría, no aprobado |
| =:curated= | Código aprobado, estable, listo para producción |

### Regla de Seguridad

**SAFETY-01:** Prohibe que funciones =:curated= dependan de =:experimental=.

Esta regla se define en =lisa-rules.lisp=:
#+begin_src lisp
(defrule rule-safety-curation-leak ()
  (code-commit-analysis (symbol-name ?caller) (status :curated) (calls ?calls))
  =>
  (dolist (callee ?calls)
    (let* ((v (find-vertex-by-symbol-name callee))
           (data (when v (cl-graph:element v))))
      (when (and data (eq (getf data :status) :experimental))
        (assert (violation ...))))))
#+end_src

---

## Diferencia entre Commits Atómicos y Humanos

| Aspecto | Atómico | Humano |
|---------|---------|--------|
| Granularidad | Función/variable individual | Grupo de funciones |
| Frecuencia | Cada definición | Puntos de control |
| Status inicial | =:experimental= | N/A |
| Promocionable | No | Sí (a =:curated=) |
| Uso | Desarrollo continuo | Releases, milestones |

---

## Flujo de Trabajo Típico

#+begin_src lisp
;; Desarrollo continuo (commits atómicos)
(make-assert '(defun util-1 () ...))
(make-assert '(defun util-2 () ...))
(make-assert '(defun main-fun () ...))

;; Crear hito cuando el código está listo
(make-human-commit "Release v1.0: Utilities complete")

;; Ahora todas las funciones son :curated
;; y pueden ser usadas por otro código :curated
#+end_src

---

## Integración con Otros Módulos

- =main.lisp=: Proporciona =*atomic-history-graph*= y =*human-history-graph*=
- =global-audit.lisp=: Audita hitos para análisis global
- =dump-source-code.lisp=: Dump de código desde hitos
- =make-image.lisp=: Reconstruye imagen desde hitos

---

## Ejemplo de Uso

#+begin_src lisp
;; Verificar si hay cambios pendientes
(has-pending-changes-p)

;; Crear hito con todos los cambios recientes
(make-human-commit "Feature X implemented")

;; Crear hito manualmente para símbolos específicos
(make-human-commit "Core utilities v1" 
                  '(util-1 util-2 helper-fn))

;; Ver historial de hitos
(show-project-milestones)
#+end_src

---

## Notas

- Los hitos forman una cadena cronológica en =*human-history-graph*=
- Al crear un hito, todos los commits atómicos incluidos pasan a =:curated=
- Los hitos permiten reconstruir la imagen del sistema desde código aprobado
- El dashboard global (=show-health-evolution=) opera a nivel de hitos
