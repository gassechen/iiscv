# Documentación de lisa-rules.lisp

## Descripción General

Define las reglas de auditoría de calidad, seguridad y confiabilidad del sistema IISCV. Este archivo contiene el motor de inferencia basado en LISA que evalúa cada commit atómico contra múltiples criterios.

## Ubicación

=src/lisa-rules.lisp=

## Paquete

=iiscv=

## Dependencias

- LISA (sistema experto)
- =lisa-rules-aux-fn.lisp= (funciones auxiliares)
- =global-audit.lisp= (reglas globales)

## Estado

Completamente documentado

---

## Templates LISA

### code-commit-analysis

Template principal que representa el análisis estático y lógico de una definición de código.

#+begin_src lisp
(deftemplate code-commit-analysis ()
  (slot commit-uuid)          
  (slot symbol-name)          
  (slot symbol-type)          
  (slot body-length)          
  (slot has-docstring-p)      
  (slot magic-numbers)        
  (slot unused-parameters)    
  (slot cyclomatic-complexity)
  (slot uses-unsafe-execution-p) 
  (slot uses-implementation-specific-symbols-p) 
  (slot contains-heavy-consing-loop-p)
  (slot style-critiques)
  (slot is-redefining-core-symbol-p)
  (slot calls)
  (slot status)              
  (slot has-side-effects-p)      
  (slot returns-constant-nil-p)  
  (slot has-dead-code-p)         
  (slot mutated-symbols)         
  (slot is-predicate-p)
  (slot is-recursive-p)
  (slot has-unbounded-loop-p)
  (slot assertion-count)
  (slot definition-form))
#+end_src

### goal

Template de control para gestionar razonamiento multi-paso (Análisis de Impacto, Validación Lógica).

#+begin_src lisp
(deftemplate goal ()
  (slot type)      ;; audit, validate-logic, trace-impact
  (slot target)   ;; nombre del símbolo
  (slot status))  ;; active, approved, rejected, failed
#+end_src

### violation

Template generado cuando una regla de calidad o seguridad es activada.

#+begin_src lisp
(deftemplate violation ()
  (slot rule-id) 
  (slot severity)  ;; :info, :warning, :error
  (slot message)
  (slot score))
#+end_src

---

## Variables Globales

| Variable | Tipo | Descripción |
|----------|------|-------------|
| =*audit-violations*= | list | Lista global para recolectar mensajes de reglas durante un commit |

---

## Reglas de Calidad (Quality & Maintainability)

### rule-1-1-high-cyclomatic-complexity

**ID:** 1.1  
**Severidad:** error  
**Score:** 10  
**Descripción:** Detecta complejidad ciclomática > 7 usando métrica McCabe.

---

### rule-1-2-function-too-long

**ID:** 1.2  
**Severidad:** warning  
**Score:** 5  
**Descripción:** Funciones que exceden 25 líneas.

---

### rule-1-3-magic-number-usage

**ID:** 1.3  
**Severidad:** warning  
**Score:** 4-10  
**Descripción:** Números mágicos detectados en el código.

---

### rule-1-4-unused-parameters

**ID:** 1.4  
**Severidad:** warning  
**Score:** 3+  
**Descripción:** Parámetros declarados pero no usados en el cuerpo.

---

### rule-1-5-heavy-consing-loop

**ID:** 1.5  
**Severidad:** warning  
**Score:** 8  
**Descripción:** Asignación de memoria (consing) dentro de loops.

---

### rule-1-6-variable-mutation

**ID:** 1.6  
**Severidad:** warning  
**Score:** 4+  
**Descripción:** Variables mutadas con setf, setq, incf, etc.

---

### rule-1-7-constant-nil-return

**ID:** 1.7  
**Severidad:** warning  
**Score:** 5  
**Descripción:** Funciones (no predicados) que siempre retornan NIL.

---

### rule-5-1-missing-docstring

**ID:** 5.1  
**Severidad:** info  
**Score:** 1  
**Descripción:** Definiciones sin documentación.

---

### rule-idiomatic-lisp-style

**ID:** IDIOMATIC-01  
**Severidad:** warning  
**Score:** 3  
**Descripción:** Recomendar estilo usando lisp-critic.

---

## Reglas de Seguridad (Security & Reliability)

### rule-2-2-internal-redefinition

**ID:** 2.2  
**Severidad:** info  
**Score:** 2  
**Descripción:** Detecta cuando una función ya existe en el historial.

---

### rule-3-1-unsafe-command-execution

**ID:** 3.1  
**Severidad:** error  
**Score:** 20  
**Descripción:** Detecta ejecución de comandos externos (uiop:run-program).

---

### rule-6-1-implementation-specific-symbols

**ID:** 6.1  
**Severidad:** warning  
**Score:** 8  
**Descripción:** Uso de símbolos específicos de implementación (SB-*, ASDF, UIOP).

---

### rule-safety-curation-leak

**ID:** SAFETY-01  
**Severidad:** error  
**Score:** 15  
**Descripción:** Prohíbe que funciones =:curated= dependan de =:experimental=.

---

## Reglas NASA JPL (Power of Ten)

### rule-logic-unreachable-code

**ID:** LOGIC-02  
**Severidad:** error  
**Score:** 12  
**Descripción:** Código inalcanzable detectado.

---

### rule-nasa-01-no-recursion

**ID:** NASA-01  
**Severidad:** warning  
**Score:** 7  
**Descripción:** Recursión prohibida en código de alta integridad.

---

### rule-nasa-05-assertion-density

**ID:** NASA-05  
**Severidad:** warning  
**Score:** 6  
**Descripción:** Densidad de assertions = 0 en funciones > 10 líneas.

---

## Reglas de Impacto (Impact Analysis)

### rule-trigger-impact

**ID:** -  
**Severidad:** -  
**Descripción:** Detecta redefiniciones y activa análisis de impacto.

---

### rule-process-impact

**ID:** LOGIC-IMPACT  
**Severidad:** warning  
**Score:** 2+  
**Descripción:** Navega el grafo para encontrar todos los dependientes activos.

---

## Sistema de Bridge

### rule-bridge-violations

**Propósito:** Intercepta hechos de violación y los push a la lista global =*audit-violations*=.

---

## Sistema de Aprobación (Backward Chaining)

### rule-trigger-logic-audit

Activa validación lógica para cada commit.

---

### rule-init-audit-goal

Crea un goal de tipo =audit= para cada símbolo.

---

### rule-evaluate-muro-threshold

Evalúa si el score total supera =*iiscv-tolerance*=:
- Si score <= tolerancia → status = approved
- Si score > tolerancia → status = rejected

---

### rule-finalize-commit-to-graph

Si el goal de auditoría y validación lógica están ambos =approved=, llama a =make-atomic-commit=.

---

## Reglas de Integridad (Rove)

### rule-fire-test-constant-integrity

Valida constantes y variables globales.

### rule-fire-test-dependency-integrity

Procesa dependencias (ql:quickload).

### rule-fire-test-rove-execution

Verifica integridad compilando en el contexto del paquete.

---

## Flujo de Ejecución

1. =make-assert= extrae métricas
2. =analyze-commit-and-assert= crea hecho =code-commit-analysis=
3. LISA infiere y genera hechos =violation=
4. =rule-bridge-violations= recolecta en =*audit-violations*=
5. =rule-init-audit-goal= crea meta de auditoría
6. =rule-evaluate-muro-threshold= decide approve/reject
7. Si approved, =rule-finalize-commit-to-graph= ejecuta =make-atomic-commit=

---

## Integración con Otros Módulos

- =main.lisp=: Llama a =analyze-commit-and-assert=
- =global-audit.lisp=: Reglas globales (ciclos, SPOF)
- =utility-fn.lisp=: Sensores de análisis
- =register-commit-type.lisp=: Tipos de commit soportados

---

## Ejemplo de Traza

#+begin_src lisp
;; Al hacer commit de una función
(make-assert '(defun slow-func (x)
                (loop for i from 1 to 100
                      collect i)))

;; LISA infiere:
;; - rule-1-2-function-too-long (score: 5)
;; - rule-nasa-05-assertion-density (score: 6)
;; Score total: 11

;; Si *iiscv-tolerance* = 10:
;; - rule-evaluate-muro-threshold → rejected
;; - No se ejecuta make-atomic-commit
#+end_src
