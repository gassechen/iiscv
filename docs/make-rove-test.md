# Documentación de make-rove-test.lisp

## Descripción General

Proporciona integración con el framework de testing Rove. Genera tests automáticamente para cada commit y permite ejecutar auditorías completas.

## Ubicación

=src/make-rove-test.lisp=

## Paquete

=iiscv=

## Dependencias

- Rove (framework de testing)
- ASDF (rutas de proyecto)

## Estado

Completamente documentado

---

## Funciones Principales

### make-rove-test-form (commit-uuid form)

**Propósito:** Envuelve una forma Lisp en un test Rceso:**
1. Genera nombreove.

**Pro de test desde UUID
2. Crea forma =deftest= con =rove:ok=

**Parámetros:**
- =commit-uuid=: UUID del commit
- =form=: Forma Lisp a testear

**Retorna:**
```lisp
(rove:deftest :COMMIT-<UUID>-TEST
  (rove:ok (eval ',form) "The form should evaluate without error."))
```

---

### make-file-commit (commit-uuid form)

**Propósito:** Escribe un archivo de test Rove para un commit.

**Proceso:**
1. Genera path =audits/<uuid>.lisp=
2. Crea directorio si no existe
3. Escribe el test form

**Parámetros:**
- =commit-uuid=: UUID del commit
- =form=: Forma Lisp

**Retorna:** NIL

---

### run-all-audits ()

**Propósito:** Ejecuta todos los tests de auditoría cargados en el sistema.

**Proceso:**
1. Verifica que exista el directorio =audits/=
2. Carga todos los archivos =*.lisp= del directorio
3. Ejecuta la suite de Rove

**Retorna:** Resultados de la suite

**Dependencia:** Require que =make-file-commit= haya generado los archivos primero.

---

## Flujo de Trabajo

```lisp
make-assert(form)
    │
    ▼
make-atomic-commit(form)
    │
    ▼
make-file-commit(uuid, form)  ;; Genera audits/uuid.lisp
    │
    ▼
run-all-audits()  ;; Carga y ejecuta todos los tests
```

---

## Estructura de Archivo Generado

```lisp
;; Archivo: audits/550e8400-e29b-41d4-a716-446655440000.lisp

(rove:deftest :COMMIT-550E8400-E29B-41D4-A716-446655440000-TEST
  (rove:ok (eval '(defun foo (x) (+ x 1))) "The form should evaluate without error."))
```

---

## Directorio de Auditorías

Ubicación default: =<proyecto>/audits/=

Para IISCV: =~/quicklisp/local-projects/iiscv/audits/=

---

## Integración con Otros Módulos

- =main.lisp=: Genera commits atómicos
- =make-human-commits.lisp=: Agrupa en hitos
- =lisa-rules.lisp=: Reglas de validación

---

## Ejemplo de Uso

```lisp
;; Después de hacer commits
(make-assert '(defun test-fn (x) (+ x 1)))

;; Generar test
(make-file-commit "550e8400-e29b-41d4-a716-446655440000" 
                  '(defun test-fn (x) (+ x 1)))

;; Ejecutar todos los tests
(run-all-audits)
```

---

## Notas

- Los tests Rove verifican que las formas compilen y evalúen sin error
- No es un mecanismo de testing de funcionalidad, sino de integridad
- Útil para regression testing después de reconstruir la imagen
- Los archivos en =audits/= Persisten entre sesiones
