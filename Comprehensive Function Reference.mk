---

# IISCV System: Comprehensive Function Reference

This document provides a detailed breakdown of every function in the IISCV system, explaining its purpose, how it works, and its role in the overall project.

---

## 1. Core System Functions

These functions are the heart of the system, responsible for the automatic, auditable development loop.

### `iiscv-repl`

* **What it does:** Starts the interactive, auditable development environment.
* **Purpose:** This is your main entry point. It wraps the standard Lisp REPL in a loop that automatically intercepts and processes any top-level definition forms you enter. By using this REPL, you ensure all your work is automatically committed and audited.

### `make-commit`

* **What it does:** Creates a new in-memory commit for a given Lisp definition form.
* **Purpose:** This function is the cornerstone of the entire system. It takes a form like `(defun my-func ...)` and generates a unique UUID for it, stores the form and metadata in the `*history*` hash table, updates the `*function-to-uuid-map*` to track the latest version, and calls `make-file-commit` to create the external audit file.

### `make-file-commit`

* **What it does:** Writes a Rove-compatible test file for a specific commit.
* **Purpose:** This function creates the **external audit trail**. It takes a commit's data and writes a `.lisp` file that contains a `rove:deftest` wrapping the original Lisp form. This allows you to externally and independently verify that the code from a specific commit still evaluates without an error.

---

## 2. History and Resilience Functions

These functions provide the ability to save, load, and rebuild the project's state, making it resilient to restarts or system failures.

### `dump-history-to-file`

* **What it does:** Saves the entire in-memory commit history to a file.
* **Purpose:** Since the `*history*` hash table is in-memory, it will be lost if you restart Lisp. This function serializes the table's contents into a Lisp file, allowing you to persist your entire development journal to disk.

### `load-history-from-file`

* **What it does:** Restores the in-memory history from a saved file.
* **Purpose:** This function loads the Lisp file created by `dump-history-to-file`, populating the `*history*` hash table and restoring the audit log to its previous state.

### `rebuild-image-from-history`

* **What it does:** Re-evaluates every committed form to rebuild the entire Lisp image.
* **Purpose:** After loading your history, your Lisp image still doesn't know about the functions you created. This function iterates through every commit in the restored `*history*` table and re-evaluates each Lisp form, effectively rebuilding your entire program from its audited source code.

---

## 3. Auditing and Debugging Functions

These functions are your primary tools for verifying code integrity and debugging past versions.

### `run-all-audits`

* **What it does:** Executes all audit tests saved to disk.
* **Purpose:** This is your primary **quality assurance tool**. It runs every single `rove:deftest` file created by `make-file-commit`, giving you a clear report on the status of your entire project history. If a change introduced a regression, this command will find it.

### `get-last-uuid-by-name`

* **What it does:** Retrieves the UUID of the last committed version of a function.
* **Purpose:** This function acts as a **lookup tool** for your codebase. It uses the `*function-to-uuid-map*` to quickly find the `UUID` of the most recent version of a function. This is critical for getting a known-good version of a function when an error occurs.

### `get-commit-form`

* **What it does:** Retrieves the original Lisp code from a commit.
* **Purpose:** This function is for **source code recovery**. Once you have a `UUID` (for instance, from `get-last-uuid-by-name`), you can use this function to get the exact Lisp form that was committed, allowing you to inspect it or paste it into your REPL for a bug fix.

### `make-rove-test-form`

* **What it does:** A helper function that wraps a Lisp form in a `rove:deftest`.
* **Purpose:** This function automates the creation of auditable tests. It ensures that every commit is written to a file in a standardized, testable format that `rove` can execute.

---

## 4. Utility and Helper Functions

These are internal functions that support the core logic of the system.

* `get-docstring-type`: Determines the documentation type of a form (`function`, `variable`, etc.).
* `get-history`: Returns the in-memory history hash table.
* `get-commit`: Retrieves a specific commit from the history by its `UUID`.
* `get-last-commit`: Returns the data of the most recent commit.
* `get-function-lambda`: A helper to get the lambda form of a function.
* `hash-commit`: A placeholder for a cryptographic hashing function.
* `browse-history`: Displays a human-readable summary of the commit history.
* `make-state-snapshot`: An additional function to audit the current state of variables.
