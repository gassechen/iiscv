# Documentación de main.lisp

## Descripción General

Archivo principal que define la estructura core del sistema IISCV. Contiene la orquestación central, el estado global y las funciones principales de commit atómico.

## Ubicación

=src/main.lisp=

## Paquete

=iiscv=

## Estado

Completamente documentado

---

## Variables Globales

| Variable | Tipo | Descripción |
|----------|------|-------------|
| =*iiscv-tolerance*= | integer | Puntuación máxima de violaciones permitida para que un átomo entre al historial. Default: 10 |
| =*function-to-uuid-map*= | hash-table | Mapea nombres de funciones (PACKAGE::NAME) al UUID del último commit |
| =*atomic-history-graph*= | cl-graph:dot-graph | Grafo para almacenar todos los commits atómicos (historial máquina) |
| =*human-history-graph*= | cl-graph:dot-graph | Grafo para almacenar hitos de nivel superior (historial humano) |
| =*last-atomic-commit-uuid*= | string | UUID del último commit atómico para enlace cronológico |
| =*current-human-commit*= | string | UUID del último hito humano |

---

## Funciones Principales

### make-assert (definition-form)

**Propósito:** Audita el código antes de commitear. Si pasa el umbral de tolerancia, llama a =make-atomic-commit=.

**Proceso:**
1. Extrae el tipo de commit usando =get-commit-type=
2. Genera un UUID único
3. Extrae métricas del código:
   - Llamadas (=extract-calls=)
   - Docstring (=get-docstring=)
   - Cuerpo (=get-body-forms=)
   - Símbolos mutados (=extract-mutated-symbols=)
   - Complejidad ciclomática (=calculate-cyclomatic-complexity=)
   - Números mágicos (=find-magic-numbers=)
   - Parámetros no usados (=find-unused-parameters=)
   - Ejecución insegura (=find-unsafe-execution-forms=)
   - Consing pesado (=contains-heavy-consing-loop-p=)
   - Símbolos específicos de implementación (=find-implementation-specific-symbols=)
   - Código muerto (=has-dead-code-p=)
   - Recursividad (=is-recursive-p=)
   - Densidad de assertions (=count-assertions=)
   - Loops no acotados (=has-unbounded-loops-p=)
4. Llama a =analyze-commit-and-assert= para alimentar el motor LISA
5. Imprime reporte de violaciones

**Retorna:** UUID del commit si pasa la auditoría, nil si es rechazado

---

### make-atomic-commit (definition-form)

**Propósito:** Compila, evalúa y registra la definición en el grafo atómico.

**Proceso:**
1. Extrae nombre y FQN (Fully Qualified Name) del símbolo
2. Genera UUID único
3. Compila la definición
4. Si la compilación falla, retorna nil
5. Si pasa, evalúa la definición
6. Crea vértice en =*atomic-history-graph*= con datos:
   - UUID
   - symbol-name
   - source-form
   - status (:experimental)
   - calls
   - message (docstring)
   - timestamp
   - rules-violations
7. Crea arista desde el commit anterior
8. Actualiza =*function-to-uuid-map*=
9. Imprime confirmación

**Retorna:** UUID del commit

---

## Funciones Auxiliares

| Función | Descripción |
|---------|-------------|
| =clear-all-commits= | Resetea todos los grafos y registros |
| =violation-score-t= | Extrae el score de una violación (4to elemento) |
| =calculate-total-score= | Suma todos los scores de violaciones |
| =get-severity-from-violation= | Extrae la severidad de una violación |
| =filter-violations-by-severity= | Filtra violaciones por nivel (:error, :warning, :info) |
| =sort-violations-by-score= | Ordena violaciones de mayor a menor score |

---

## Formato de Violaciones

Las violaciones se representan como listas:
#+begin_src lisp
(message severity rule-id score)
#+end_src

Ejemplo:
#+begin_src lisp
("High cyclomatic complexity (8) found in function 'FOO'." :error "1.1" 10)
#+end_src

---

## Integración con Otros Módulos

- =lisa-rules.lisp=: Proporciona =analyze-commit-and-assert=
- =utility-fn.lisp=: Proporciona funciones de extracción de métricas
- =register-commit-type.lisp=: Proporciona =get-commit-type=
- =lisa-rules-aux-fn.lisp=: Proporciona todos los sensores de análisis

---

## Ejemplo de Uso

#+begin_src lisp
;; Auditar y commitear una función
(make-assert '(defun mi-funcion (x y)
                "Suma dos números"
                (+ x y)))

;; Ver estado actual
(clear-all-commits)
#+end_src

---

## Notas

- El threshold de tolerancia (=*iiscv-tolerance*=) controla qué tan estricto es el sistema
- Las violaciones con score > tolerancia causan que el commit sea rechazado
- Cada commit atómico se enlaza con el anterior formando una cadena cronológica
- Las redefiniciones se detectan consultando =*function-to-uuid-map*=
