# Documentación de make-image.lisp

## Descripción General

Módulo de gestión de imágenes Lisp. Permite guardar estados del sistema (desarrollo y producción) y reconstruir la imagen desde el historial de commits.

## Ubicación

=src/make-image.lisp=

## Paquete

=iiscv=

## Dependencias

- SB-EXT, CCL, ABCL, ECL (implementación específica)
- ASDF (rutas)

## Estado

Completamente documentado

---

## Funciones de Estado

### has-pending-changes-p ()

**Propósito:** Retorna T si hay commits atómicos sin commit humano asociado.

**Proceso:**
1. Obtiene timestamp del último commit humano
2. Itera sobre todos los vértices atómicos
3. Retorna T si algún timestamp es posterior

**Retorna:** Boolean

---

## Funciones de Saving

### save-development-image (path)

**Propósito:** Guarda una imagen de desarrollo con todo el historial y datos de auditoría.

**Restricciones:**
- No permite guardar si hay cambios pendientes

**Implementaciones soportadas:**
- SBCL: =sb-ext:save-lisp-and-die=
- CCL: =ccl:save-application=
- ABCL: =ext:save-application=
- ECL: =ext:save-executable=

**Parámetros:**
- =path=: Ruta donde guardar la imagen

---

### save-production-image (path)

**Propósito:** Crea una imagen de producción ligera reconstruyendo desde commits humanos.

**Proceso:**
1. Verifica cambios pendientes
2. Llama a =rebuild-image-from-human-history=
3. Guarda imagen limpia

**Diferencia con desarrollo:** No incluye grafos de historial.

---

## Funciones de Reconstrucción

### rebuild-image-from-human-history ()

**Propósito:** Reconstruye la imagen del sistema evaluando los commits atómicos vinculados al historial humano.

**Proceso:**
1. Verifica que =*human-history-graph*= no esté vacío
2. Itera sobre hitos en orden topológico
3. Para cada hito, procesa sus atomic-uuids
4. Evalúa cada forma fuente
5. Maneja errores gracefully

**Retorna:** NIL

**Nota:** Solo incluye código =:curated=.

---

### rebuild-image-from-atomic-history ()

**Propósito:** Reconstruye la imagen evaluando cada commit atómico en el historial.

**Proceso:**
1. Verifica que =*atomic-history-graph*= no esté vacío
2. Itera sobre todos los vértices atómicos en orden topológico
3. Evalúa cada forma fuente

**Diferencia:** Incluye TODO el código, incluyendo =:experimental=.

**Uso:** Disaster recovery

---

## Comparación de Reconstrucciones

| Función | Incluye | Uso |
|---------|---------|-----|
| rebuild-image-from-human-history | Solo :curated | Producción |
| rebuild-image-from-atomic-history | Todo (:experimental + :curated) | Recuperación |

---

## Flujo de Saving

#+begin
Cambios pendientes?
    │
    ├──► Sí → Error, hacer make-human-commit primero
    │
    └──► No
          │
          ├──► save-development-image
          │    └──► Guarda TODO (grafos + código)
          │
          └──► save-production-image
               └──► rebuild + guarda solo código
#+end

---

## Integración con Otros Módulos

- =main.lisp=: Proporciona =*atomic-history-graph*= y =*human-history-graph*=
- =make-human-commits.lisp=: Crea hitos
- =dump-source-code.lisp=: Genera archivos fuente

---

## Ejemplo de Uso

#+begin_src lisp
;; Verificar cambios pendientes
(has-pending-changes-p)

;; Guardar imagen de desarrollo (completa)
(save-development-image #p"/tmp/iiscv-dev.image")

;; Guardar imagen de producción (limpia)
(save-production-image #p"/tmp/iiscv-prod.image")

;; Reconstruir desde historial curado
(rebuild-image-from-human-history)

;; Reconstruir desde historial completo
(rebuild-image-from-atomic-history)
#+end_src

---

## Notas

- Las imágenes son específicas de la implementación Lisp
- Imágenes de desarrollo incluyen los grafos de historial
- Imágenes de producción son más pequeñas (solo código)
- La reconstrucción permite recuperar el estado del sistema desde los grafos
