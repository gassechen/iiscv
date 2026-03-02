Vamos a simular un pequeño sistema de Cálculo de Nómina. Tiraremos estas funciones una por una:

1. El Átomo Base: CALCULAR-IMPUESTO
Es una función pura, no depende de nadie.

(make-assert '(defun calcular-impuesto (monto)
  "Calcula el 21% de impuesto."
  (* monto 0.21)))
  

IISCV> (make-assert '(defun calcular-impuesto (monto)
  "Calcula el 21% de impuesto."
  (* monto 0.21)))

[LISA-FORENSIC] Análisis de 'CALCULAR-IMPUESTO' completado. Score: 4
[ROVE-AUDIT] Verificando integridad de CALCULAR-IMPUESTO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] CALCULAR-IMPUESTO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'CALCULAR-IMPUESTO' en el grafo.
WARNING: redefining IISCV::CALCULAR-IMPUESTO in DEFUN
[IISCV-OK] Átomo registrado: 24AB76B0-32C2-490E-9E72-75D29183D086

[AUDIT] CALCULAR-IMPUESTO | Violations: 1 (0 errors, 1 warnings) | Total Score: 4
  [1.3] ( 4 pts) WARNING: Se detectaron 1 números mágicos ((0.21)) en 'CALCULAR-IMPUESTO'.


2. El Intermediario: GENERAR-RECIBO
Esta llama directamente a la anterior (posición car).

(make-assert '(defun generar-recibo (empleado sueldo)
  "Crea un string con el recibo y el impuesto calculado."
  (let ((impuesto (calcular-impuesto sueldo)))
    (format nil "Empleado: ~A | Neto: ~A | Impuesto: ~A" 
            empleado sueldo impuesto))))
			
			
IISCV> (make-assert '(defun generar-recibo (empleado sueldo)
  "Crea un string con el recibo y el impuesto calculado."
  (let ((impuesto (calcular-impuesto sueldo)))
    (format nil "Empleado: ~A | Neto: ~A | Impuesto: ~A" 
            empleado sueldo impuesto))))

[LISA-FORENSIC] Análisis de 'GENERAR-RECIBO' completado. Score: 0
[ROVE-AUDIT] Verificando integridad de GENERAR-RECIBO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] GENERAR-RECIBO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'GENERAR-RECIBO' en el grafo.
WARNING: redefining IISCV::GENERAR-RECIBO in DEFUN
[IISCV-OK] Átomo registrado: 29475A10-3370-4389-96A3-1AD5F4D48D45

[AUDIT] GENERAR-RECIBO | Violations: 0 (0 errors, 0 warnings) | Total Score: 0


3. La Maestra (Orden Superior): PROCESAR-PLANILLA
Esta es la prueba de fuego para tu extract-calls recursivo, porque usa #'CALCULAR-IMPUESTO como argumento.

(make-assert '(defun procesar-planilla (lista-sueldos)
  "Aplica el cálculo de impuestos a toda una lista."
  (mapcar #'calcular-impuesto lista-sueldos)))
  
  
IISCV> (make-assert '(defun procesar-planilla (lista-sueldos)
  "Aplica el cálculo de impuestos a toda una lista."
  (mapcar #'calcular-impuesto lista-sueldos)))

[LISA-FORENSIC] Análisis de 'PROCESAR-PLANILLA' completado. Score: 0
[ROVE-AUDIT] Verificando integridad de PROCESAR-PLANILLA...
[INFO] Compilando en contexto de paquete: IISCV
[OK] PROCESAR-PLANILLA validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'PROCESAR-PLANILLA' en el grafo.
WARNING: redefining IISCV::PROCESAR-PLANILLA in DEFUN
[IISCV-OK] Átomo registrado: 2BFEC36C-31D5-4B63-85A3-0D5FE08BC7E4

[AUDIT] PROCESAR-PLANILLA | Violations: 0 (0 errors, 0 warnings) | Total Score: 0


IISCV> (find-dependents-in-history 'calcular-impuesto)


(PROCESAR-PLANILLA GENERAR-RECIBO)

IISCV> (audit-atomic-history)

--- Atomic History Audit (Blockchain) ---

* Atomic Commit: 24AB76B0-32C2-490E-9E72-75D29183D086
  Message: Calcula el 21% de impuesto.
  Form: (DEFUN CALCULAR-IMPUESTO (MONTO)
          Calcula el 21% de impuesto.
          (* MONTO 0.21))
  Timestamp: 3981456548
  Violations detected: (Se detectaron 1 números mágicos ((0.21)) en 'CALCULAR-IMPUESTO'.)

* Atomic Commit: 29475A10-3370-4389-96A3-1AD5F4D48D45
  Message: Crea un string con el recibo y el impuesto calculado.
  Form: (DEFUN GENERAR-RECIBO (EMPLEADO SUELDO)
          Crea un string con el recibo y el impuesto calculado.
          (LET ((IMPUESTO (CALCULAR-IMPUESTO SUELDO)))
            (FORMAT NIL Empleado: ~A | Neto: ~A | Impuesto: ~A EMPLEADO SUELDO
                    IMPUESTO)))
  Timestamp: 3981456631
  Violations detected: NIL

* Atomic Commit: 2BFEC36C-31D5-4B63-85A3-0D5FE08BC7E4
  Message: Aplica el cálculo de impuestos a toda una lista.
  Form: (DEFUN PROCESAR-PLANILLA (LISTA-SUELDOS)
          Aplica el cálculo de impuestos a toda una lista.
          (MAPCAR #'CALCULAR-IMPUESTO LISTA-SUELDOS))
  Timestamp: 3981456685
  Violations detected: NIL
  
  
El Test de Estrés: Capa 2 vs Indirect-Calls
Vamos a crear una función que no menciona a CALCULAR-IMPUESTO en la posición de llamada, sino que la recibe y la ejecuta mediante FUNCALL.

1. Invocación por FUNCALL

(make-assert '(defun ejecutor-dinamico (monto)
  "Ejecuta el cálculo usando funcall."
  (funcall #'calcular-impuesto monto)))
  
IISCV> (make-assert '(defun ejecutor-dinamico (monto)
  "Ejecuta el cálculo usando funcall."
  (funcall #'calcular-impuesto monto)))

[LISA-FORENSIC] Análisis de 'EJECUTOR-DINAMICO' completado. Score: 0
[ROVE-AUDIT] Verificando integridad de EJECUTOR-DINAMICO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] EJECUTOR-DINAMICO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'EJECUTOR-DINAMICO' en el grafo.
WARNING: redefining IISCV::EJECUTOR-DINAMICO in DEFUN
[IISCV-OK] Átomo registrado: 826452F6-3685-429B-8415-4590D779079E

[AUDIT] EJECUTOR-DINAMICO | Violations: 0 (0 errors, 0 warnings) | Total Score: 0

2. Invocación por APPLY

(make-assert '(defun ejecutor-masivo (lista-montos)
  "Ejecuta el cálculo usando apply (solo el primero por simplicidad)."
  (apply #'calcular-impuesto (list (car lista-montos)))))
  

IISCV> (make-assert '(defun ejecutor-masivo (lista-montos)
  "Ejecuta el cálculo usando apply (solo el primero por simplicidad)."
  (apply #'calcular-impuesto (list (car lista-montos)))))

[LISA-FORENSIC] Análisis de 'EJECUTOR-MASIVO' completado. Score: 3
[ROVE-AUDIT] Verificando integridad de EJECUTOR-MASIVO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] EJECUTOR-MASIVO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'EJECUTOR-MASIVO' en el grafo.
WARNING: redefining IISCV::EJECUTOR-MASIVO in DEFUN
[IISCV-OK] Átomo registrado: 29D6659D-4BF7-4723-9CB2-89F75A518892

[AUDIT] EJECUTOR-MASIVO | Violations: 1 (0 errors, 1 warnings) | Total Score: 3
  [IDIOMATIC-01] ( 3 pts) WARNING: Style recommendations for 'EJECUTOR-MASIVO': (APPLY (FUNCTION CALCULAR-IMPUESTO) (LIST ...)) makes a list for no
reason. How can you call (FUNCTION CALCULAR-IMPUESTO) with those
arguments directly?


IISCV> (find-dependents-in-history 'calcular-impuesto)

(EJECUTOR-MASIVO EJECUTOR-DINAMICO PROCESAR-PLANILLA GENERAR-RECIBO)


IISCV> (make-human-commit "TRACE TEST")
[CURATION] CALCULAR-IMPUESTO promoted to :CURATED.
[CURATION] GENERAR-RECIBO promoted to :CURATED.
[CURATION] PROCESAR-PLANILLA promoted to :CURATED.
[CURATION] EJECUTOR-DINAMICO promoted to :CURATED.
[CURATION] EJECUTOR-MASIVO promoted to :CURATED.

[IISCV] Human commit created: TRACE TEST
"5ED86917-BC63-43E7-BEE8-FF1B0C1C61CF"


IISCV> (audit-current)

=== Ejecutando Auditoría Global ===

============================================================
[ IISCV GLOBAL AUDIT DASHBOARD ]
------------------------------------------------------------

ESTADO: GREEN - ESTABLE
RIESGO TOTAL: 15 pts
INCIDENCIAS:  1

ID         | SCORE    | DESCRIPCIÓN
------------------------------------------------------------
GLOBAL-06  | 15       | SPOF: 'CALCULAR-IMPUESTO' tiene 4 dependientes.
============================================================

El Test de "Infección de Dependencias"
Vamos a intentar actualizar CALCULAR-IMPUESTO con algo que sea válido en Lisp, pero que rompa la lógica para sus dependientes (por ejemplo, devolviendo un string en lugar de un número).

IISCV> (make-assert '(defun calcular-impuesto (monto)
  "SABOTAJE: Ahora devuelvo basura para romper los dependientes."
  (declare (ignore monto))
  "ERROR_DE_SISTEMA"))

[LISA-FORENSIC] Análisis de 'CALCULAR-IMPUESTO' completado. Score: 7
[ROVE-AUDIT] Verificando integridad de CALCULAR-IMPUESTO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] CALCULAR-IMPUESTO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'CALCULAR-IMPUESTO' en el grafo.
WARNING: redefining IISCV::CALCULAR-IMPUESTO in DEFUN
[IISCV-OK] Átomo registrado: 6538F38F-2E2B-412C-8ABC-A5D612EADC4F

[AUDIT] CALCULAR-IMPUESTO | Violations: 2 (0 errors, 1 warnings) | Total Score: 7
  [LOGIC-IMPACT] ( 5 pts) WARNING: Forensic Impact: 4 funciones 
(EJECUTOR-MASIVO, EJECUTOR-DINAMICO, PROCESAR-PLANILLA, GENERAR-RECIBO) 
dependen de 'CALCULAR-IMPUESTO' y requieren revisión.
  [2.2] ( 2 pts) INFO: Mutation: The FUNCTION 'CALCULAR-IMPUESTO' has been updated in the history.


IISCV> (make-assert '(defun calcular-impuesto (monto)
  "Calcula el impuesto usando un factor de escala definido."
  (declare (type number monto))
  (let ((factor-iva 0.21)) ; O mejor aún, una variable global
    (* monto factor-iva))))

[LISA-FORENSIC] Análisis de 'CALCULAR-IMPUESTO' completado. Score: 7
[ROVE-AUDIT] Verificando integridad de CALCULAR-IMPUESTO...
[INFO] Compilando en contexto de paquete: IISCV
[OK] CALCULAR-IMPUESTO validado correctamente.
[MURO-OK] Paso de guardia exitoso. Registrando átomo 'CALCULAR-IMPUESTO' en el grafo.
WARNING: redefining IISCV::CALCULAR-IMPUESTO in DEFUN
[IISCV-OK] Átomo registrado: 18CE26BD-81D5-47A4-A33F-45B431E99AA2

[AUDIT] CALCULAR-IMPUESTO | Violations: 2 (0 errors, 1 warnings) | Total Score: 7
  [LOGIC-IMPACT] ( 5 pts) WARNING: Forensic Impact: 4 funciones 
(EJECUTOR-MASIVO, EJECUTOR-DINAMICO, PROCESAR-PLANILLA, GENERAR-RECIBO) 
dependen de 'CALCULAR-IMPUESTO' y requieren revisión.
  [2.2] ( 2 pts) INFO: Mutation: The FUNCTION 'CALCULAR-IMPUESTO' has been updated in the history.

IISCV> (audit-current)

=== Ejecutando Auditoría Global ===

============================================================
[ IISCV GLOBAL AUDIT DASHBOARD ]
------------------------------------------------------------

ESTADO: GREEN - ESTABLE
RIESGO TOTAL: 15 pts
INCIDENCIAS:  1

ID         | SCORE    | DESCRIPCIÓN
------------------------------------------------------------
GLOBAL-06  | 15       | SPOF: 'CALCULAR-IMPUESTO' tiene 4 dependientes.
============================================================


IISCV> (show-health-evolution)

--- EVOLUCIÓN DE SALUD (VÍA CACHE) ---
HITO                           | SCORE      | TENDENCIA
-------------------------------------------------------
TRACE TEST                     | 15         | INIT
GLOB TEST 1                    | 15         | ESTABLE
GLOB TEST 2                    | 15         | ESTABLE
-------------------------------------------------------
