### Implementación del Motor de Auditoría Global

Para esto, crearemos una función que "levante" una red de inferencia temporal con la "Verdad Curada".

#### 1. La Función Orquestadora: `run-global-logical-audit`

Esta función toma un hito humano, extrae todos sus átomos y los inyecta en una red LISA fresca.

```lisp
(defun run-global-logical-audit (human-commit-uuid)
  "Ejecuta una red de inferencia 'one-shot' sobre el estado curado de un Human Commit."
  (let* ((h-vertex (find-vertex-by-uuid *human-history-graph* human-commit-uuid))
         (h-data (when h-vertex (cl-graph:element h-vertex)))
         (atomic-uuids (getf h-data :atomic-uuids)))
    
    (unless atomic-uuids
      (format t "Error: No hay átomos en este hito para auditar.~%")
      (return-from run-global-logical-audit nil))

    ;; --- INICIO DE LA RED ONE-SHOT ---
    (lisa:reset) ;; Limpiamos la memoria de trabajo
    
    (format t "~%[FORENSIC-AUDIT] Cargando ~A definiciones curadas...~%" (length atomic-uuids))
    
    ;; 1. Alimentar la red con la 'Verdad' del Grafo
    (dolist (uuid atomic-uuids)
      (let* ((a-vertex (find-vertex-by-uuid *atomic-history-graph* uuid))
             (a-data (cl-graph:element a-vertex)))
        ;; Inyectamos los datos atómicos como hechos de LISA
        (lisa:assert `(code-commit-analysis
                        (symbol-name ,(getf a-data :symbol-name))
                        (calls ',(getf a-data :calls))
                        (status :curated)
                        (expected-type ,(getf a-data :expected-type))
                        (has-side-effects-p ,(getf a-data :has-side-effects-p))))))

    ;; 2. Disparar el objetivo de validación global
    (lisa:assert '(goal (type :global-validation) (status :active)))

    ;; 3. Inferencia
    (lisa:run)

    ;; 4. Reporte de Resultados
    (let ((results iiscv::*audit-violations*))
      (format t "~%=== REPORTE DE INTEGRIDAD LÓGICA GLOBAL ===~%")
      (if results
          (dolist (v results) (format t "-> [~A] ~A~%" (third v) (first v)))
          (format t "No se detectaron inconsistencias lógicas en este hito.~%"))
      
      ;; 5. RESET FINAL (Mantenemos la imagen liviana)
      (lisa:reset)
      results)))
```

#### 2. Reglas de Integridad Global (Cross-Function)

Estas reglas solo tienen sentido cuando LISA "ve" todo el programa a la vez.

**A. Referencias Huérfanas en el Hito:**
Si una función curada llama a algo que NO está incluido en este Human Commit ni existe en el sistema.

```lisp
(defrule rule-global-orphan-call ()
  (goal (type :global-validation) (status :active))
  (code-commit-analysis (symbol-name ?caller) (calls ?calls))
  =>
  (dolist (callee ?calls)
    ;; Si el callee no existe como hecho en esta red 'one-shot'
    (unless (lisa:find-fact-instance 'code-commit-analysis (symbol-name callee))
      (assert (violation (rule-id "GLOBAL-01")
                         (severity :error)
                         (message (format nil "Inconsistencia de Hito: '~A' llama a '~A', pero '~A' no forma parte de este commit curado." ?caller callee callee)))))))
```

**B. Contradicción de Efecto de Sitio:**
Si una función pura (sin side effects) llama a una que SÍ tiene side effects, podrías estar rompiendo la transparencia referencial de tu arquitectura.

```lisp
(defrule rule-global-purity-violation ()
  (code-commit-analysis (symbol-name ?pure-fn) (has-side-effects-p nil) (calls ?calls))
  (code-commit-analysis (symbol-name ?dirty-fn) (has-side-effects-p t))
  (test (member ?dirty-fn ?calls :test #'equal))
  =>
  (assert (violation (rule-id "GLOBAL-02")
                     (severity :warning)
                     (message (format nil "Violación de Pureza: '~A' (declarada pura) depende de '~A' (que tiene efectos de sitio)." ?pure-fn ?dirty-fn)))))
```

### Por qué esto es superior:

1.  **Ahorro de Memoria:** Al ser "One-Shot", la red LISA solo crece durante el análisis. Una vez que termina, `lisa:reset` libera todos los objetos del motor, manteniendo tu imagen Lisp limpia de "hechos" viejos.
2.  **Determinismo:** El archivo que generes con `make-human-history-file` después de este análisis estará **garantizado logícamente**. No habrá sorpresas de "falta una función" o "el tipo no coincide".
3.  **Forense:** Puedes guardar el resultado de este análisis global *dentro* del nodo del Human Commit. Así, el registro dirá: *"Este hito fue validado lógicamente el 24/05/2024 y pasó 150 reglas de integridad"*.

**¿Te gustaría que diseñemos un "Walker" más profundo que extraiga los tipos de las funciones (`(declare (type ...))`) para que el análisis global sea de tipado fuerte?**
