# Documentación de global-audit.lisp

## Descripción General

Este módulo implementa la auditoría a nivel de hito (human commit) completo. A diferencia de =lisa-rules.lisp= que evalúa commits atómicos individualmente, =global-audit.lisp= analiza el grafo completo para detectar problemas arquitecturales como ciclos y puntos únicos de fallo (SPOF).

## Ubicación

=src/global-audit.lisp=

## Paquete

=iiscv=

## Dependencias

- cl-graph (manipulación de grafos)
- LISA (motor de reglas)

## Estado

Completamente documentado

---

## Variables Globales

| Variable | Tipo | Descripción |
|----------|------|-------------|
| =*audit-history-cache*= | hash-table | Almacena scores de auditoría globales indexados por UUID del commit humano |

---

## Templates LISA (Auditoría Global)

### sys-func

Representa una función en el snapshot del sistema.

```lisp
(deftemplate sys-func ()
  (slot name)
  (slot arity))
```

### sys-call

Representa una llamada entre funciones.

```lisp
(deftemplate sys-call ()
  (slot from)
  (slot to))
```

### sys-goal

Meta de auditoría global.

```lisp
(deftemplate sys-goal ()
  (slot status))
```

### sys-spof-score

Puntuación de Single Point Of Failure.

```lisp
(deftemplate sys-spof-score ()
  (slot func-name)
  (slot count)
  (slot processed-parents))
```

---

## Funciones Principales

### run-global-audit (human-commit-uuid)

**Propósito:** Ejecuta auditoría global sobre un hito completo.

**Proceso:**
1. Resetea LISA
2. Obtiene el vértice del hito humano
3. Construye snapshot del sistema hasta ese hito:
   - Itera sobre el =*human-history-graph*= en orden topológico
   - Para cada =atomic-uuid=, obtiene el commit atómico
   - Construye tabla hash =snapshot-table= con nombre → datos
4. Carga hechos en LISA:
   - =sys-func= para cada función
   - =sys-call= para cada llamada (solo si no es función de CL)
5. Ejecuta =lisa:run=
6. Actualiza cache de auditoría
7. Reporta resultados

**Parámetros:**
- =human-commit-uuid=: UUID del hito a auditar

**Retorna:** Lista de violaciones

---

### update-audit-cache (human-uuid violations)

**Propósito:** Actualiza el cache de auditoría con el score total.

**Parámetros:**
- =human-uuid=: UUID del hito
- =violations=: Lista de violaciones

**Retorna:** Score total

---

## Reglas LISA Globales

### rule-cycle

**ID:** GLOBAL-01  
**Severidad:** warning  
**Score:** 10  
**Descripción:** Detecta ciclosmutuos entre funciones (A → B → A).

**Condición:**
- =sys-goal= con status :active
- =sys-call= de ?a a ?b
- =sys-call= de ?b a ?a
- =a < b= (para evitar duplicados)

---

### rule-init-spof

**Propósito:** Inicializa contador de SPOF para cada función.

---

### rule-accumulate-spof

**Propósito:** Acumula el número de padres que llaman a una función.

---

### rule-report-spof-critical

**ID:** GLOBAL-06  
**Severidad:** error  
**Score:** 15  
**Descripción:** Single Point of Failure con ≥3 funciones dependientes.

---

## Dashboard y Reportes

### report-global-results (violations)

**Propósito:** Imprime el Dashboard Global.

**Formato de salida:**
```
============================================================
[ IISCV GLOBAL AUDIT DASHBOARD ]
============================================================

ESTADO: GREEN - ESTABLE
RIESGO TOTAL: 25 pts
INCIDENCIAS:  3

ID         | SCORE   | DESCRIPCIÓN
------------------------------------------------------------
GLOBAL-01  | 10      | Ciclo: FOO <-> BAR
GLOBAL-06  | 15      | SPOF: 'BAZ' tiene 3 dependientes.
============================================================
```

**Estados:**
| Score | Estado |
|-------|--------|
| > 40 | RED - CRÍTICO |
| > 15 | YELLOW - AVISO |
| ≤ 15 | GREEN - ESTABLE |

---

### show-health-evolution

**Propósito:** Muestra la tendencia de salud del proyecto a lo largo de los hitos.

**Formato de salida:**
```
--- EVOLUCIÓN DE SALUD (VÍA CACHE) ---
HITO                         | SCORE     | TENDENCIA
-------------------------------------------------------
Release v1.0                 | 25        | INIT
Add feature X                | 30        | EMPEORANDO
Fix bugs                    | 20        | MEJORANDO
-------------------------------------------------------
```

---

### rebuild-audit-history

**Propósito:** Recorre todos los hitos humanos y regenera el cache de auditoría.

**Proceso:**
1. Limpia el cache
2. Itera sobre =*human-history-graph*= en orden topológico
3. Ejecuta =run-global-audit= para cada hito
4. Llama a =show-health-evolution=

---

### rebuild-audit-history-quiet

**Propósito:** Reconstrucción con reporte de progreso minimalista.

**Diferencia:** No imprime detalles de violaciones, solo el score y tendencia.

---

### audit-current

**Propósito:** Ejecuta auditoría global sobre el último hito humano.

**Equivalente a:**
```lisp
(run-global-audit *current-human-commit*)
```

---

## Flujo de Auditoría Global

```_src
make-human-commit("Release v1.0")
    │
    ▼
run-global-audit(commit-uuid)
    │
    ├──► Construir snapshot desde human-history
    ├──► Cargar sys-func y sys-call en LISA
    ├──► Ejecutar reglas globales
    │    │
    │    ├──► rule-cycle (detectar ciclos)
    │    └──► rule-report-spof-critical (SPOF)
    │
    ├──► update-audit-cache
    └──► report-global-results

show-health-evolution()
    │
    └──► Mostrar tendencia por hito
```

---

## Integración con Otros Módulos

- =main.lisp=: Proporciona =*atomic-history-graph*= y =*human-history-graph*=
- =make-human-commits.lisp=: Crea hitos que luego se auditan
- =reports.lisp=: Proporciona =triage-atomic-debt-report=
- =utility-fn.lisp=: Proporciona =clean-name-for-lisa=, =calculate-arity=

---

## Ejemplo de Uso

```lisp
;; Crear un hito
(make-human-commit "Release 1.0")

;; Ejecutar auditoría global
(run-global-audit *current-human-commit*)

;; Ver dashboard
(report-global-results *audit-violations*)

;; Ver evolución de salud
(show-health-evolution)

;; Reconstruuir todo el historial
(rebuild-audit-history)
```

---

## Notas

- La auditoría global se ejecuta a nivel de =human-commit=, no =atomic-commit=
- El cache =*audit-history-cache*= permite consultas rápidas de tendencias
- Las reglas globales operan sobre el snapshot completo, no sobre funciones individuales
- El análisis de SPOF es acumulativo: cuenta cuántas funciones dependen de una función específica
