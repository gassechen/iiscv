
# IISCV: The Forensic Truth Engine for Live Lisp Images

> **"The machine is the repository."** IISCV eliminates "source code amnesia" in Lisp images, transforming them into auditable organisms that encapsulate their own genetic history.

---

## 1. What is IISCV?
IISCV (**Immutable and Curated Version Control System**) is a version control and code auditing system that resides **inside** the Lisp image.

Unlike Git, which manages external text files, IISCV manages **"Living Knowledge."** It intercepts every definition (`defun`, `defclass`, etc.), audits it using a built-in expert system, and registers it immutably within a memory-resident graph.

## 2. The Three Core Pillars

### 2.1. Atomic History Graph (Code Blockchain)
Every time you evaluate a function, IISCV doesn't just compile it; it creates an **Atomic Commit**.
*   **Immutability:** Every version of the source code is stored with a unique UUID and timestamp.
*   **Traceability:** Nodes are cryptographically linked. You can see exactly how a function evolved over time.
*   **Forensics:** If an image fails, the actual source code is inside the RAM, ready for post-mortem inspection.

### 2.2. Real-Time Quality Auditing (LISA Expert System)
IISCV acts as a "Code Customs Checkpoint." Before a function is registered, the **LISA** inference engine analyzes its anatomy:
*   **Security:** Detects dangerous external executions.
*   **Quality:** Identifies magic numbers, oversized functions, or missing documentation.
*   **Efficiency:** Warns about memory allocation (*heavy consing*) in critical loops.
*   **Outcome:** The system rejects or flags code that fails to meet industrial safety standards.

### 2.3. Intelligent Image Management
IISCV distinguishes between development "noise" and production "purity."
*   **Human Commits:** Group related atomic changes into high-level, readable **Milestones**.
*   **Clean Reconstruction:** Generates a production image (`prod.core`) by re-evaluating only the curated history. This eliminates experimental "garbage" and ensures a lightweight, deterministic binary.

---

## 3. Workflow

1.  **Interception:** The developer (or an AI via MCP) sends code to the REPL.
2.  **Auditing:** LISA analyzes the S-Expression. Any violations are reported immediately.
3.  **Registration:** An immutable node is created in the `*atomic-history-graph*`.
4.  **Consolidation:** A human engineer validates changes using `make-human-commit`.
5.  **Deployment:** A production image is generated through deterministic reconstruction.

---

## 4. Main API

| Function | Purpose |
| :--- | :--- |
| `make-atomic-commit` | Audits and registers an individual definition. |
| `make-human-commit` | Consolidates atomic commits into a human milestone. |
| `audit-atomic-history` | Displays the complete "Black Box" history of the machine. |
| `show-project-milestones` | Displays the curated history for human review. |
| `run-all-audits` | Executes automated tests (Rove) on recorded commits. |
| `save-production-image` | Reconstructs and saves an optimized production binary. |

---

## 5. Use Cases
IISCV is designed for **Critical Systems** (Industrial, Finance, Aerospace) that require:
*   **Absolute Trust:** Knowing exactly what code is running inside an image that has been live for months.
*   **Remote Maintenance:** Repairing machines in inaccessible locations (e.g., Andean weather stations) where external repositories are unavailable.
*   **Mandatory Auditing:** Complying with regulations that demand a total forensic trail of every "hot-fix."

---

## 6. AI-Augmented Engineering (MCP Integration)
IISCV is fully compatible with the **Model Context Protocol (MCP)**. This allows autonomous AI agents to:
1.  Propose code changes via an audited channel.
2.  **Self-correct** by reading LISA's violation reports.
3.  Operate within the safety rails of a professional industrial environment.

---

### "IISCV turns the Lisp 'Save Game' into a Forensic-Grade Industrial Ledger."



-----------------------------------------------------------------------------------------------------------------------


**IISCV como el motor forense y de auditoría**. 

---

# IISCV: The Forensic Truth Engine for Live Lisp Images

> **La máquina es el repositorio.** IISCV elimina la "amnesia" de las imágenes Lisp, transformándolas en organismos audatables que contienen su propia historia genética.

---

## 1. ¿Qué es IISCV?
IISCV (*Immutable and Curated Version Control System*) es un sistema de control de versiones y auditoría de código que reside **dentro** de la imagen de Lisp. 

A diferencia de Git, que gestiona archivos de texto externos, IISCV gestiona **"Conocimiento Vivo"**. Intercepta cada definición (`defun`, `defclass`, etc.), la audita mediante un sistema experto y la registra de forma inmutable en un grafo de memoria.

## 2. Los 3 Pilares Fundamentales

### 2.1. Grafo de Historia Atómica (Blockchain de Código)
Cada vez que evalúas una función, IISCV no solo la compila; crea un **Commit Atómico**.
*   **Inmutabilidad:** Cada versión del código fuente se guarda con un UUID y un timestamp.
*   **Trazabilidad:** Los nodos están enlazados. Puedes ver exactamente cómo evolucionó una función a través del tiempo.
*   **Forense:** Si la imagen falla, el código fuente real está dentro de la RAM, listo para ser inspeccionado.

### 2.2. Auditoría en Tiempo Real (IA Lógica con LISA)
IISCV actúa como una "Aduana de Código". Antes de que una función se registre, el motor de inferencia **LISA** la analiza:
*   **Seguridad:** Detecta ejecuciones externas peligrosas.
*   **Calidad:** Identifica números mágicos, funciones demasiado largas o falta de documentación.
*   **Eficiencia:** Advierte sobre el uso excesivo de memoria (*heavy consing*) en bucles críticos.
*   **Resultado:** El sistema rechaza o marca el código que no cumple con los estándares industriales.

### 2.3. Gestión Inteligente de Imágenes
IISCV distingue entre el "ruido" del desarrollo y la "pureza" de la producción.
*   **Human Commits:** Agrupan micro-cambios atómicos en hitos lógicos (Milestones).
*   **Reconstrucción Limpia:** Permite generar una imagen de producción (`prod.core`) volviendo a ejecutar solo la historia curada, eliminando basura experimental y garantizando un binario ligero y predecible.

---

## 3. Flujo de Trabajo (Workflow)

1.  **Interceptación:** El programador (o una IA vía MCP) envía código al REPL.
2.  **Auditoría:** LISA analiza la S-Expression. Si hay violaciones, se notifican al instante.
3.  **Registro:** Se crea un nodo inmutable en el `*atomic-history-graph*`.
4.  **Consolidación:** El humano valida los cambios con `make-human-commit`.
5.  **Despliegue:** Se genera la imagen de producción mediante reconstrucción determinista.

---

## 4. Funciones Principales (API)

| Función | Propósito |
| :--- | :--- |
| `make-atomic-commit` | Audita y registra una definición individual. |
| `make-human-commit` | Consolida commits atómicos en un hito humano. |
| `audit-atomic-history` | Muestra la "Caja Negra" completa de la máquina. |
| `show-project-milestones`| Muestra la historia curada para humanos. |
| `run-all-audits` | Ejecuta tests automáticos (Rove) sobre los commits. |
| `save-production-image` | Reconstruye y salva un binario optimizado para planta. |

---

## 5. ¿Para quién es esto?
IISCV es para ingenieros de **Sistemas Críticos** (Industria, Finanzas, Aeroespacial) que necesitan:
*   **Confianza Absoluta:** Saber exactamente qué hay dentro de una imagen que lleva meses corriendo.
*   **Mantenimiento Remoto:** Reparar máquinas en lugares inaccesibles donde no hay acceso a repositorios externos.
*   **Auditoría Obligatoria:** Cumplir con normativas que exigen trazabilidad total de cada cambio en caliente.

---

### "IISCV convierte el 'Save Game' de Lisp en un Registro Forense de Grado Industrial."
