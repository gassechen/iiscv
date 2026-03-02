# Documentación de reports.lisp

## Descripción General

Módulo de visualización y reportes. Proporciona funciones para mostrar el historial de commits, deuda técnica y herramientas de reparación.

## Ubicación

=src/reports.lisp=

## Paquete

=iiscv=

## Dependencias

- =main.lisp= (grafos)
- =utility-fn.lisp= (consulta)
- =lisa-rules-aux-fn.lisp= (análisis)

## Estado

Completamente documentado

---

## Funciones de Visualización

### show-atomic-commit ()

**Propósito:** Retorna todos los vértices del grafo atómico.

**Retorna:** Lista de vértices

---

### show-human-commit ()

**Propósito:** Retorna todos los vértices del grafo humano.

**Retorna:** Lista de vértices

---

### show-project-milestones ()

**Propósito:** Muestra el historial curado del proyecto navegando los commits humanos.

**Formato de salida:**
```
--- Project Milestones (Human History) ---

* Milestone: Release v1.0
  UUID: 550e8400-e29b-41d4-a716-446655440000
  Timestamp: 3979401818
  Atomic Changes: ("uuid1" "uuid2" ...)

* Milestone: Fix bugs
  UUID: ...
--------------------------------------------
```

---

### audit-atomic-history ()

**Propósito:** Muestra el historial completo y detallado navegando los commits atómicos.

**Formato de salida:**
```
--- Atomic History Audit (Blockchain) ---

* Atomic Commit: 550e8400-e29b-41d4-a716-446655440000
  Message: No docstring provided.
  Form: (DEFUN FOO (X) (+ X 1))
  Timestamp: 3979401818
  Violations detected: (("High complexity" ...))
...
```

---

## Reportes de Deuda Técnica

### triage-atomic-debt-report ()

**Propósito:** Genera reporte técnico de los átomos con deuda, ordenados por score de mayor a menor.

**Proceso:**
1. Itera sobre todos los vértices atómicos
2. Calcula score total de cada átomo (suma de scores de violaciones)
3. Filtra átomos con score > 0
4. Ordena descendente por score
5. Imprime reporte detallado

**Formato de salida:**
```
==================================================================
   IISCV FORENSIC REPORT: DEUDA TÉCNICA DEL GRAFO ATÓMICO
==================================================================

[SCORE:  25] Átomo: MY-FUNCTION
             UUID: 550e8400-e29b-41d4-a716-446655440000
             [-] High complexity (10 pts)
             [-] Unused parameters (8 pts)
------------------------------------------------------------------

[SCORE:  12] Átomo: HELPER-FN
             UUID: ...
             [-] Magic numbers (7 pts)
------------------------------------------------------------------

Reporte finalizado. Usa (sanitize-atom "MY-FUNCTION") para el primer objetivo.
==================================================================
```

**Retorna:** NIL

---

## Herramientas de Reparación

### sanitize-atom (function-name)

**Propósito:** Sana una función específica recuperando su forma del grafo.

**Proceso:**
1. Recupera los datos del commit usando =get-source-form=
2. Muestra las violaciones actuales
3. Muestra el código actual en el grafo
4. Solicita nueva definición al usuario
5. Ejecuta =make-assert= con la nueva forma

**Parámetros:**
- =function-name=: Nombre de la función (string o símbolo)

**Ejemplo de uso:**
```lisp
(sanitize-atom "MI-FUNCION")
;; Muestra:
;; ================================================
;; [AUDIT-REPAIR] Saneando átomo: MI-FUNCION
;; Violaciones actuales: ("High complexity" "Unused params")
;; ------------------------------------------------
;; Código en el grafo:
;; (DEFUN MI-FUNCION (X Y Z) ...)
;; ------------------------------------------------
;; Pega la nueva definición para sobreescribir:
;; (defun mi-funcion (x) (+ x 1))  ; Usuario ingresa esto
;;
;; [MURO] Re-evaluando integridad de MI-FUNCION...
```

**Retorna:** NIL

---

## Integración con Otros Módulos

- =main.lisp=: Proporciona =*atomic-history-graph*= y =*human-history-graph*=
- =utility-fn.lisp=: Proporciona =get-source-form=
- =global-audit.lisp=: Dashboard de salud

---

## Ejemplo de Uso

```lisp
;; Ver hitos del proyecto
(show-project-milestones)

;; Ver historial atómico
(audit-atomic-history)

;; Ver deuda técnica
(triage-atomic-debt-report)

;; Sanear una función
(sanitize-atom 'mi-funcion-problematica)
```

---

## Notas

- =show-project-milestones= opera sobre el grafo humano (curado)
- =audit-atomic-history= opera sobre el grafo atómico (completo)
- =triage-atomic-debt-report= ayuda a priorizar refactoring
- =sanitize-atom= permite corregir código directamente desde el REPL
