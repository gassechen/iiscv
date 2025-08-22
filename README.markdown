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


```lisp 
IISCV> (iiscv-repl)

IISCV-R> (defun add-two (x)
             "Adds 2 to a number."
             (+ x 2))
WARNING: redefining IISCV::ADD-TWO in DEFUN

ADD-TWO 
IISCV-R> (human-commit "Added the initial add-two function." 'add-two)

"27423BA5-BA6E-4996-BE6F-4A904BF704D2" 
IISCV-R> (get-source-form "iiscv::add-two")

(DEFUN ADD-TWO (X) "Adds 2 to a number." (+ X 2)) 
IISCV-R> (defun add-two (x)
             "Corrected function: adds 3 to a number."
             (+ x 3))
WARNING: redefining IISCV::ADD-TWO in DEFUN

ADD-TWO 
IISCV-R> (human-commit "Corrected add-two to add 3 instead of 2." 'add-two)

"2B03DECF-B21E-45C4-8776-CC0F8F6D1029" 
IISCV-R> (get-source-form "iiscv::add-two")

(DEFUN ADD-TWO (X) "Corrected function: adds 3 to a number." (+ X 3)) 
IISCV-R> (cl-graph:vertexes *human-history-graph*)

(#<(UUID D30E2723-5E97-4926-9D87-CDA78F8CB0F0 MESSAGE
    Se agregaron funciones de suma y producto básicas ATOMIC-UUIDS
    (14AD8CAF-6186-46C0-96CB-6344C25CB92F A40E9C9D-4322-483D-94AB-74709644B9A2)
    TIMESTAMP 3964862788)>
 #<(UUID F397D644-2C65-46A1-BECD-9A4EB85D1BE4 MESSAGE
    Se agregó validación a la función de suma ATOMIC-UUIDS
    (1B572C8C-E5AF-469F-92A8-1D71CFE0E73F A40E9C9D-4322-483D-94AB-74709644B9A2)
    TIMESTAMP 3964862922)>
 #<D30E2723-5E97-4926-9D87-CDA78F8CB0F0>
 #<F397D644-2C65-46A1-BECD-9A4EB85D1BE4>
 #<(UUID 433B5978-1AFA-4C22-8C77-4617E4FC6B77 MESSAGE
    Se agregó la función de división y se actualizó la función de producto
    ATOMIC-UUIDS
    (61B1973E-FA71-4DFE-8AED-A4BAFF8F5660 3EC4379F-A233-40EE-9706-46C30B4286A1)
    TIMESTAMP 3964863389)>
 #<433B5978-1AFA-4C22-8C77-4617E4FC6B77>
 #<(UUID 0036F216-02A2-4F8A-A067-B8AB307614D2 MESSAGE
    Se agregó y corrigió la función TEST-GLOBAL-VARIABLE y su variable global.
    ATOMIC-UUIDS
    (2CCF968C-7491-41D5-96C5-4EEE5FC7A612 D3AAD0FA-E19D-42F6-8D65-BE93088DAD43)
    TIMESTAMP 3964863914)>
 #<0036F216-02A2-4F8A-A067-B8AB307614D2>
 #<(UUID AF9E60E7-F5B1-4805-BA3E-ECDA50DCB0D1 MESSAGE
    Added the initial add-two function. ATOMIC-UUIDS
    (0B863120-6FB4-4CDF-96BD-DD488C8775E1) TIMESTAMP 3964868339)>
 #<AF9E60E7-F5B1-4805-BA3E-ECDA50DCB0D1>
 #<(UUID 27423BA5-BA6E-4996-BE6F-4A904BF704D2 MESSAGE
    Added the initial add-two function. ATOMIC-UUIDS
    (B948A001-562B-4008-8C22-719DA7BB133A) TIMESTAMP 3964868513)>
 #<27423BA5-BA6E-4996-BE6F-4A904BF704D2>
 #<(UUID 2B03DECF-B21E-45C4-8776-CC0F8F6D1029 MESSAGE
    Corrected add-two to add 3 instead of 2. ATOMIC-UUIDS
    (2D9DEC9A-2AE3-4F15-8891-8B49A74BB7A2) TIMESTAMP 3964868575)>
 #<2B03DECF-B21E-45C4-8776-CC0F8F6D1029>) 
IISCV-R> (cl-graph:vertexes *atomic-history-graph*)

(#<(UUID 14AD8CAF-6186-46C0-96CB-6344C25CB92F SOURCE-FORM
    (DEFUN SUM (A B) Calculates the sum of two numbers. (+ A B)) MESSAGE
    Calculates the sum of two numbers. TIMESTAMP 3964862599)>
 #<(UUID A40E9C9D-4322-483D-94AB-74709644B9A2 SOURCE-FORM
    (DEFUN PRODUCT (A B) Calculates the product of two numbers. (* A B))
    MESSAGE Calculates the product of two numbers. TIMESTAMP 3964862685)>
 #<14AD8CAF-6186-46C0-96CB-6344C25CB92F>
 #<A40E9C9D-4322-483D-94AB-74709644B9A2>
 #<(UUID 1B572C8C-E5AF-469F-92A8-1D71CFE0E73F SOURCE-FORM
    (DEFUN SUM (A B)
      Calculates the sum of two numbers, ensuring they are valid.
      (UNLESS (AND (NUMBERP A) (NUMBERP B)) (ERROR Inputs must be numbers.))
      (+ A B))
    MESSAGE Calculates the sum of two numbers, ensuring they are valid.
    TIMESTAMP 3964862908)>
 #<1B572C8C-E5AF-469F-92A8-1D71CFE0E73F>
 #<(UUID 61B1973E-FA71-4DFE-8AED-A4BAFF8F5660 SOURCE-FORM
    (DEFUN DIVIDE (A B)
      Divides the first number by the second. Prevents division by zero.
      (WHEN (= B 0) (ERROR Cannot divide by zero.))
      (/ A B))
    MESSAGE Divides the first number by the second. Prevents division by zero.
    TIMESTAMP 3964863159)>
 #<61B1973E-FA71-4DFE-8AED-A4BAFF8F5660>
 #<(UUID 3EC4379F-A233-40EE-9706-46C30B4286A1 SOURCE-FORM
    (DEFUN PRODUCT (A B)
      Calculates the product of two numbers, with a helpful message.
      (FORMAT T Calculating product...~%)
      (* A B))
    MESSAGE Calculates the product of two numbers, with a helpful message.
    TIMESTAMP 3964863166)>
 #<3EC4379F-A233-40EE-9706-46C30B4286A1>
 #<(UUID B2376EDA-5E4B-4451-8EFC-EC6699832796 SOURCE-FORM
    (DEFUN TEST-GLOBAL-VARIABLE ()
      A function that tries to use a global variable that doesn't exist.
      (FORMAT T The value of *test-global* is: ~A~% *TEST-GLOBAL*))
    MESSAGE A function that tries to use a global variable that doesn't exist.
    TIMESTAMP 3964863700)>
 #<B2376EDA-5E4B-4451-8EFC-EC6699832796>
 #<(UUID D3AAD0FA-E19D-42F6-8D65-BE93088DAD43 SOURCE-FORM
    (DEFVAR *TEST-GLOBAL*
      Hello, world!
      A global variable for testing purposes.)
    MESSAGE A global variable for testing purposes. TIMESTAMP 3964863728)>
 #<D3AAD0FA-E19D-42F6-8D65-BE93088DAD43>
 #<(UUID B4B48C71-4893-4B60-971E-0CD2BA45AA92 SOURCE-FORM
    (DEFUN TEST-GLOBAL-VARIABLE ()
      A corrected function with a deliberately malformed let.
      (LET ((X 10)))
      (FORMAT T The value of *test-global* is: ~A~% *TEST-GLOBAL*))
    MESSAGE A corrected function with a deliberately malformed let. TIMESTAMP
    3964863745)>
 #<B4B48C71-4893-4B60-971E-0CD2BA45AA92>
 #<(UUID 2CCF968C-7491-41D5-96C5-4EEE5FC7A612 SOURCE-FORM
    (DEFUN TEST-GLOBAL-VARIABLE ()
      A fully corrected function that should compile and run without error.
      (LET ((X 10))
        (FORMAT T The value of *test-global* is: ~A~% *TEST-GLOBAL*)
        X))
    MESSAGE
    A fully corrected function that should compile and run without error.
    TIMESTAMP 3964863760)>
 #<2CCF968C-7491-41D5-96C5-4EEE5FC7A612>
 #<(UUID 91C50307-0858-432A-84B6-10525134DA0A SOURCE-FORM
    (DEFUN SQUARE (X) Calculates the square of a number. (* X X)) MESSAGE
    Calculates the square of a number. TIMESTAMP 3964868125)>
 #<91C50307-0858-432A-84B6-10525134DA0A>
 #<(UUID 0B863120-6FB4-4CDF-96BD-DD488C8775E1 SOURCE-FORM
    (DEFUN ADD-TWO (X) Adds 2 to a number. (+ X 2)) MESSAGE Adds 2 to a number.
    TIMESTAMP 3964868280)>
 #<0B863120-6FB4-4CDF-96BD-DD488C8775E1>
 #<(UUID B948A001-562B-4008-8C22-719DA7BB133A SOURCE-FORM
    (DEFUN ADD-TWO (X) Adds 2 to a number. (+ X 2)) MESSAGE Adds 2 to a number.
    TIMESTAMP 3964868499)>
 #<B948A001-562B-4008-8C22-719DA7BB133A>
 #<(UUID 2D9DEC9A-2AE3-4F15-8891-8B49A74BB7A2 SOURCE-FORM
    (DEFUN ADD-TWO (X) Corrected function: adds 3 to a number. (+ X 3)) MESSAGE
    Corrected function: adds 3 to a number. TIMESTAMP 3964868565)>
 #<2D9DEC9A-2AE3-4F15-8891-8B49A74BB7A2>) 
IISCV-R> 


IISCV> (show-project-milestones)

--- Project Milestones (Human History) ---

* Milestone: Added the initial add-two function.
  UUID: 0198F786-9E41-41FD-BA50-640F997E89B1
  Timestamp: 3964880619
  Atomic Changes: (7DE2102A-0B1F-4D78-A092-D6BEAA422846)

* Milestone: Added multiplication and subtraction functions.
  UUID: 77EED9BB-9BEC-491E-8FDF-C849D75F43C4
  Timestamp: 3964880655
  Atomic Changes: (825377BA-3EA8-4985-9A13-F49E9B2E3AD2
                   B27EC5AB-0AB5-47D8-A82A-83F846FE6C22)

* Milestone: Modified add-two to be more robust.
  UUID: E6D01FB5-5FA5-4135-9C34-C67A08A72EBC
  Timestamp: 3964880678
  Atomic Changes: (65652141-BADE-4FA3-A63B-9A04ED64DB3B)
--------------------------------------------
NIL
IISCV> 
