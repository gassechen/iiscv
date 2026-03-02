# Índice de Documentación IISCV

## Visión General

Documentación técnica detallada de cada módulo del sistema IISCV (Immutable and Curated Version Control).

---

## Estructura de Archivos

```
docs/
├── main.md                       # Core Orchestrator
├── lisa-rules.md                  # Reglas de auditoría LISA
├── global-audit.md                # Auditoría a nivel de hito
├── lisa-rules-aux-fn.md          # Sensores de análisis
├── utility-fn.md                  # Funciones utilitarias
├── make-human-commits.md         # Sistema de hitos
├── dump-source-code.md           # Exportación de código
├── register-commit-type.md       # Registro de tipos
├── make-rove-test.md             # Integrove
├── make-image.md                 # Gestión de imágenes
ación R├── reports.md                    # Visualización y reportes
├── repl-r.md                     # REPL interactivo
└── make-class-commits.md         # Modificación de clases
```

---

## Módulos por Categoría

### Core y Orquestación

| Archivo | Descripción |
|---------|-------------|
| [[main.md][main.lisp]] | Core Orchestrator - Variables globales, make-assert, make-atomic-commit |
| [[register-commit-type.md][register-commit-type.lisp]] | Registro central de tipos de commit |

### Motor de Auditoría

| Archivo | Descripción |
|---------|-------------|
| [[lisa-rules.md][lisa-rules.lisp]] | Reglas de calidad, seguridad y NASA JPL |
| [[global-audit.md][global-audit.lisp]] | Auditoría global (ciclos, SPOF) |
| [[lisa-rules-aux-fn.md][lisa-rules-aux-fn.lisp]] | Sensores de análisis estático |

### Sistema de Commits

| Archivo | Descripción |
|---------|-------------|
| [[make-human-commits.md][make-human-commits.lisp]] | Hitos y curación de código |
| [[make-class-commits.md][make-class-commits.lisp]] | Modificación de clases |

### Utilidades y Acceso

| Archivo | Descripción |
|---------|-------------|
| [[utility-fn.md][utility-fn.lisp]] | Consulta y búsqueda en grafos |
| [[reports.md][reports.lisp]] | Visualización y reportes |
| [[dump-source-code.md][dump-source-code.lisp]] | Exportación de código |

### Integración y Runtime

| Archivo | Descripción |
|---------|-------------|
| [[repl-r.md][repl-r.lisp]] | REPL interactivo |
| [[make-rove-test.md][make-rove-test.lisp]] | Integración con Rove |
| [[make-image.md][make-image.lisp]] | Gestión de imágenes |

---

## Flujo de Datos

```
┌─────────────────────────────────────────────────────────────┐
│                    make-assert                               │
│                  (Auditoría inicial)                         │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                 analyze-commit-and-assert                    │
│            (Sensores → Template LISA)                        │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                      LISA Engine                             │
│    ┌──────────────────┐    ┌────────────────────┐          │
│    │  lisa-rules      │    │   global-audit     │          │
│    │  (por átomo)     │    │   (por hito)       │          │
│    └──────────────────┘    └────────────────────┘          │
└─────────────────────────┬───────────────────────────────────┘
                          │
              ┌───────────┴───────────┐
              ▼                       ▼
┌─────────────────────────┐  ┌─────────────────────────┐
│  make-atomic-commit    │  │   run-global-audit     │
│  (Grafo atómico)        │  │   (Dashboard)          │
└─────────────────────────┘  └─────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────────┐
│                  make-human-commit                           │
│              (Grafo humano + curación)                       │
└─────────────────────────────────────────────────────────────┘
```

---

## Reglas de Auditoría por Categoría

### Calidad (Quality)

| ID | Descripción | Score |
|----|-------------|-------|
| 1.1 | Complejidad ciclomática > 7 | 10 |
| 1.2 | Función > 25 líneas | 5 |
| 1.3 | Números mágicos | 4-10 |
| 1.4 | Parámetros no usados | 3+ |
| 1.5 | Heavy consing en loops | 8 |
| 1.6 | Variables mutadas | 4+ |
| 1.7 | Retorna NIL constante | 5 |
| 5.1 | Sin docstring | 1 |

### Seguridad (Security)

| ID | Descripción | Score |
|----|-------------|-------|
| 2.2 | Redefinición interna | 2 |
| 3.1 | Ejecución externa | 20 |
| 6.1 | Símbolos implementation-specific | 8 |
| SAFETY-01 | Curation Leak | 15 |

### NASA JPL

| ID | Descripción | Score |
|----|-------------|-------|
| NASA-01 | Recursión | 7 |
| NASA-05 | Baja densidad de assertions | 6 |
| LOGIC-02 | Código muerto | 12 |

### Global

| ID | Descripción | Score |
|----|-------------|-------|
| GLOBAL-01 | Ciclos | 10 |
| GLOBAL-06 | SPOF (≥3 dependientes) | 15 |

---

## APIs Principales

### Desarrollo

```lisp
;; Commit automático
(make-assert '(defun foo (x) (+ x 1)))

;; REPL interactivo
(iiscv-repl)

;; Cargar con auditoría
(iiscv-load "archivo.lisp")
```

### Hitos

```lisp
;; Crear hito
(make-human-commit "Release v1.0")

;; Ver hitos
(show-project-milestones)
```

### Auditoría

```lisp
;; Auditoría global
(run-global-audit *current-human-commit*)

;; Dashboard
(show-health-evolution)

;; Deuda técnica
(triage-atomic-debt-report)
```

### Reconstrucción

```lisp
;; Dump de código
(dump-source-code)
(dump-source-code-by-commit-type)

;; Reconstruir imagen
(rebuild-image-from-human-history)
(rebuild-image-from-atomic-history)
```

---

## Tipos de Commit

| Tipo | Formas |
|------|--------|
| function | defun, defmacro |
| variable | defvar, defparameter, defconstant |
| type | defclass, defstruct |
| slot-change | add-slot, remove-slot |
| dependency | ql:quickload |

---

## Estados de Código

| Estado | Descripción |
|--------|-------------|
| :experimental | Nuevo, en auditoría |
| :curated | Aprobado, estable |

---

## Dependencias Externas

- **cl-graph**: Manipulación de grafos
- **lisa**: Motor de reglas expertas
- **lisp-critic**: Análisis de estilo
- **uuid**: Generación de UUIDs
- **rove**: Framework de testing
- **alexandria**: Utilidades Common Lisp
- **cl-ppcre**: Expresiones regulares
