# Documentación de lisa-rules-aux-fn.lisp

## Descripción General

Contiene las funciones auxiliares de análisis forense (sensores) que extraen métricas y propiedades del código fuente. Estas funciones alimentan el motor LISA con datos para evaluar reglas de calidad.

## Ubicación

=src/lisa-rules-aux-fn.lisp=

## Paquete

=iiscv=

## Dependencias

- alexandria (utilidades)
- cl-ppcre (regex)

## Estado

Completamente documentado

---

## 1. Sensores de Cuerpo y Métricas

### get-body-forms (definition-form)

Extrae el cuerpo de una definición, manejando correctamente docstrings.

#+begin_src lisp
(get-body-forms '(defun foo (x) "doc" (bar x)))
;; => ((bar x))
#+end_src

---

### calculate-body-length (definition-form)

Cuenta el número total de átomos/llamadas en el cuerpo.

**Algoritmo:** Aplana el cuerpo y cuenta elementos.

---

### calculate-cyclomatic-complexity (definition-form)

Calcula la complejidad ciclomática (Métrica de McCabe).

**Puntos de decisión:**
- =if=, =when=, =unless=
- =cond=, =case=
- =loop=, =dolist=, =dotimes=

**Fórmula:** =+ 1 (count-decision-points body)=

---

### count-decision-points (form)

Cuenta recursivamente las estructuras de control que aumentan la complejidad.

---

## 2. Sensores de Parámetros

### get-parameters-list (definition-form)

Extrae la lista de parámetros de una definición.

---

### extract-parameter-names (params)

Extrae solo nombres de parámetros, ignorando keywords =&=.

**Resultado:** Lista de símbolos

---

### find-unused-parameters (definition-form)

Encuentra parámetros declarados pero no usados en el cuerpo.

**Algoritmo:**
1. Extrae parámetros declarados
2. Escanea el cuerpo para símbolos usados
3. Retorna diferencia

---

### find-used-symbols (form)

Encuentra y cuenta recursivamente todos los símbolos usados en una forma.

**Retorna:** Hash table con símbolo → count

---

## 3. Sensores de Seguridad y Lógica (NASA JPL)

### get-docstring (definition-form)

Extrae la docstring de una definición, saltando correctamente la lista de argumentos.

---

### has-dead-code-p (form)

Detecta ramas inalcanzables en expresiones condicionales.

**Analiza:**
- =if=, =when=, =unless=: Detecta condiciones siempre verdaderas/falsas
- =case=, =etypecase=, =ccase=: Detecta cláusulas después de =t= u =otherwise=
- =cond=: Rama terminal
- =handler-case=: Código después de =t= o =error=

---

### is-recursive-p (name form)

Detecta si una función se llama a sí misma.

**Algoritmo:**
1. Normaliza el nombre a símbolo
2. Escanea el cuerpo buscando el nombre
3. Retorna T si se encuentra

---

### count-assertions (form)

Cuenta mecanismos defensivos: =assert=, =check-type=, =error=, =warn=, =ecase=, =etypecase=.

---

### has-unbounded-loops-p (form)

Detecta loops potencialmente infinitos (LOOP sin cláusulas de salida).

**Cláusulas de salida detectadas:** =repeat=, =for=, =while=, =until=

---

### extract-mutated-symbols (form)

Detecta símbolos modificados por operadores destructivos.

**Operadores detectados:** =setf=, =setq=, =incf=, =decf=, =push=, =pop=, =nconc=

---

### is-lisp-predicate-p (name)

Verifica si un nombre sigue la convención de predicados (-P).

---

## 4. Sensores de Sistema y Estilo

### find-unsafe-execution-forms (form)

Encuentra formas asociadas con ejecución de comandos externos.

**Símbolos detectados:**
- =uiop:run-program=
- =external-program:start=

---

### find-implementation-specific-symbols (form)

Detecta símbolos de paquetes internos de implementación.

**Paquetes detectados:** =SB-*=, =ASDF=, =UIOP=

**Excepciones:** =COMMON-LISP=, =CL-USER=, =IISCV=

---

### contains-heavy-consing-loop-p (definition-form)

Detecta asignación de memoria (consing) dentro de loops.

**Operadores detectados:** =cons=, =list=, =list*=, =make-array=, =make-hash-table=

---

### clean-critic-report (raw-report)

Limpia la salida de lisp-critic (remueve líneas de guiones y whitespace).

---

## 5. Sensores de Dependencias y Grafos

### extract-calls (form &optional current-name)

Extrae todas las funciones llamadas desde una forma.

**Características:**
- Genera FQN (Fully Qualified Names)
- Excluye el nombre actual (evita auto-referencia)
- Excluye =NIL= y =T=

---

### find-dependents-in-history (function-name)

Busca funciones activas que dependen de una función dada.

**Algoritmo:**
1. Itera sobre todos los vértices del grafo atómico
2. Para cada vértice activo, verifica si =function-name= está en sus llamadas
3. Retorna lista de dependientes

---

## 6. Bridge LISA

### analyze-commit-and-assert (&key ...)

**Propósito:** Alimenta todos los datos forenses al motor LISA.

**Parámetros:**
- =uuid=: Identificador único
- =name=: Nombre del símbolo
- =symbol-type=: Tipo (function, variable, type, etc.)
- =has-docstring-p=: Boolean
- =body-length=: Entero
- =cyclomatic-complexity=: Entero
- =magic-numbers=: Lista
- =unused-parameters=: Lista
- =is-redef=: Boolean
- =calls=: Lista de llamadas
- =uses-unsafe-execution-p=: Boolean
- =contains-heavy-consing-loop-p=: Boolean
- =uses-implementation-specific-symbols-p=: Boolean
- =style-critiques=: String
- =status=: :experimental o :curated
- =is-predicate=: Boolean
- =has-dead-code=: Boolean
- =is-recursive=: Boolean
- =assertion-count=: Entero
- =has-unbounded-loop=: Boolean
- =has-side-effects=: Boolean
- =returns-constant-nil=: Boolean
- =mutated-symbols=: Lista
- =definition-form=: Forma original

**Proceso:**
1. Resetea =*audit-violations*=
2. Resetea LISA
3. Crea hecho =code-commit-analysis= con todos los datos
4. Ejecuta =lisa:run= dos veces

---

## Utilidades Auxiliares

### calculate-arity (form)

Calcula la aridad de una función (número de parámetros requeridos).

---

### cl-function-p (name-str)

Verifica si un nombre es una función nativa de Common Lisp.

---

### clean-name-for-lisa (name-thing)

Limpia nombres para uso en LISA (remueve =::=).

---

## Resumen de Sensores

| Categoría | Sensores |
|-----------|----------|
| Métricas | body-length, cyclomatic-complexity |
| Parámetros | unused-parameters, find-used-symbols |
| Lógica | dead-code, recursive, unbounded-loops |
| Seguridad | unsafe-execution, implementation-specific |
| Estilo | magic-numbers, heavy-consing, assertions |
| Dependencias | extract-calls, find-dependents |

---

## Integración con Otros Módulos

- =lisa-rules.lisp=: Consume los datos de los sensores
- =main.lisp=: Llama a todos los sensores en =make-assert=
- =register-commit-type.lisp=: Define tipos de definición

---

## Ejemplo de Uso

#+begin_src lisp
;; Analizar una función
(calculate-cyclomatic-complexity 
  '(defun ejemplo (x)
     (cond ((< x 0) -1)
           ((> x 0) 1)
           (t 0))))
;; => 3

;; Encontrar parámetros no usados
(find-unused-parameters 
  '(defun foo (x y z)
     (print x)))
;; => (Y Z)

;; Extraer llamadas
(extract-calls 
  '(defun bar ()
     (foo)
     (baz)))
;; => ("IISCV::FOO" "IISCV::BAZ")
#+end_src
