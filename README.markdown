# Project IISCV: A Common Lisp Auditable Development System

IISCV is a project aimed at **reviving the image-based development paradigm** inherent to Common Lisp, adapting it with a unique layer of internal and external auditability. This system transforms the traditional "edit-compile-run" cycle into a resilient and auditable workflow, ensuring every functional change is automatically recorded and verifiable.

***

## Recent Updates

The project has recently undergone a major refactoring to enhance its core functionality and robustness.

* **From Hash Table to Directed Graph**: The commit history system has been replaced with a **directed graph (`cl-graph:dot-graph`)** to store atomic commits (`*atomic-history-graph*`). This new structure provides a more precise representation of the project's history, paving the way for future functionalities like branching and merging.
* **Enhanced Metadata**: Each atomic commit is now a node in the graph, storing comprehensive metadata in a property list. This includes a unique **UUID**, the full source code (`:SOURCE-FORM`), a message (`:MESSAGE`), and a timestamp.
* **Robust Search Functionality**: We have developed and refined the **`get-source-form`** function, which allows you to retrieve a function's source code reliably. This function is **case-insensitive** for function names and is flexible enough to return either just the source code or the entire commit metadata.

***

## Objective and Philosophy

The primary objective of the IISCV project is to **"revive the image-based software development paradigm, inherent to Lisp, and adapt it with a layer of internal and external auditability."** This paradigm was historically surpassed by file-based systems due to the lack of robust auditing tools. The project seeks to **"solve this weak point to demonstrate that this approach is viable and superior for certain software domains."**

The central philosophy that drives this system is **"debugging and development by incremental substitution."** In contrast to the traditional "edit-compile-run" cycle, the IISCV approach is a **"continuous process of 'evaluate-debug-replace' in the live program environment."** This allows for an auditable record of every functional change, acting as a **"laboratory journal"** where **"every idea, every functional experiment, is recorded and verified."**

***

## Key Features

* **Real-time Auditing:** Automatically creates a permanent audit trail for every top-level Lisp form (`defun`, `defvar`, etc.) evaluated within its custom REPL.
* **Version Control for a Live Image:** Provides a Git-like system (`get-source-form`, `find-vertex-by-uuid`) to navigate and recover code from a live, running Lisp image.
* **Incremental Debugging:** Allows developers to debug and incrementally replace functions, leaving an auditable record of the working state before and after a fix.
* **Robustness:** Mitigates the common "Excel problem" of unrecorded changes by ensuring every functional change is a verifiable commit.

***

## Installation

1.  Ensure you have **Quicklisp** installed and configured.

2.  Clone this repository into your `quicklisp/local-projects/` directory.

```bash
git clone [https://github.com/gassechen/iiscv.git](https://github.com/gassechen/iiscv.git) quicklisp/local-projects/iiscv
