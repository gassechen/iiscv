## Project IISCV: A Common Lisp Auditable Development System

IISCV is a project aimed at **reviving the image-based development paradigm** inherent to Common Lisp, adapting it with a unique layer of internal and external auditability. This system transforms the traditional "edit-compile-run" cycle into a resilient and auditable workflow, ensuring every functional change is automatically recorded and verifiable.

-----

### Objective and Philosophy

The primary objective of the IISCV project is to **"revive the image-based software development paradigm, inherent to Lisp, and adapt it with a layer of internal and external auditability."** This paradigm was historically surpassed by file-based systems due to the lack of robust auditing tools. The project seeks to **"solve this weak point to demonstrate that this approach is viable and superior for certain software domains."**

The central philosophy that drives this system is **"debugging and development by incremental substitution."** In contrast to the traditional "edit-compile-run" cycle, the IISCV approach is a **"continuous process of 'evaluate-debug-replace' in the live program environment."** This allows for an auditable record of every functional change, acting as a **"laboratory journal"** where **"every idea, every functional experiment, is recorded and verified."**

-----

### Key Features

  * **Real-time Auditing:** Automatically creates a permanent audit trail for every top-level Lisp form (`defun`, `defvar`, etc.) evaluated within its custom REPL.
  * **Version Control for a Live Image:** Provides a Git-like system (`get-commit-form`, `get-last-uuid-by-name`) to navigate and recover code from a live, running Lisp image.
  * **Incremental Debugging:** Allows developers to debug and incrementally replace functions, leaving an auditable record of the working state before and after a fix.
  * **Robustness:** Mitigates the common "Excel problem" of unrecorded changes by ensuring every functional change is a verifiable commit.
  * **Integrated Testing:** Every commit is saved as a Rove test file, allowing for a comprehensive audit of the entire project history with a single command.

-----

### Installation

1.  Ensure you have **Quicklisp** installed and configured.

2.  Clone this repository into your `quicklisp/local-projects/` directory.

    ```bash
    git clone https://github.com/gassechen/iiscv.git quicklisp/local-projects/iiscv
    ```

3.  Load the system in your Lisp REPL.

    ```lisp
    (ql:quickload :iiscv)
    ```

-----

### Usage

1.  **Start the custom REPL:**

    ```lisp
    (iiscv:iiscv-repl)
    ```

    You will see a new prompt, ` IISCV-R>  `, indicating that you are in the auditable environment.

2.  **Define a function:**

    ```lisp
    IISCV-R> (defun my-function (x)
               "A simple function that will be audited."
               (* x 2))
    ```

    The system will automatically generate a UUID and save the code as a commit.

3.  **Run an audit:**

    ```lisp
    IISCV-R> (run-all-audits)
    ```

    This command will run all saved tests and verify that every function in your history is working as expected.

4.  **Debug incrementally:**
    If a function fails, use the following command to retrieve its last working version:

    ```lisp
    (get-commit-form (get-last-uuid-by-name 'iiscv::my-function))
    ```

    You can then correct the code and re-evaluate it to create a new, audited commit.

-----

### License

This project is open-source and available under the MIT License.

### Video Demonstration

See a full demonstration of the IISCV system in action, from incremental development and auditing to full image recovery, in this video:

[**Watch the full demo on YouTube**](https://youtu.be/SgEvuOrFJIs?si=rzuchrAkJF9J-2Kg)

