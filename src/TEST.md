This file contains a comprehensive test suite for **IISCV**. Run these definitions one by one in your REPL to verify that every single rule (Maintainability, Security, Logic, NASA, and Curation) is firing correctly.

### 0. Preparation
```lisp
(iiscv:clear-all-commits)
```

---

### 1. Maintainability & Style Rules

**Rule 1.3: Magic Numbers**
```lisp
(iiscv:make-atomic-commit
  '(defun test-magic-numbers ()
     "Should trigger Rule 1.3"
     (+ 42 999)))
```

**Rule 5.1: Missing Docstring**
```lisp
(iiscv:make-atomic-commit
  '(defun test-no-docstring (x)
     (print x)))
```

**Rule 1.1 & 1.2: High Complexity & Body Length**
```lisp
(iiscv:make-atomic-commit
  '(defun test-complex-and-long (a b c d e f g)
     "Should trigger Rule 1.1 (>10 complexity) and 1.2 (>25 lines)"
     (if a (if b (if c (if d (print 1) (print 2)))))
     (if e (if f (if g (print 3) (print 4))))
     (cond (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
     (print 1) (print 2) (print 3) (print 4) (print 5)
     (print 6) (print 7) (print 8) (print 9) (print 10)
     (print 11) (print 12) (print 13) (print 14) (print 15)
     (print 16) (print 17) (print 18) (print 19) (print 20)))
```

**Rule IDIOMATIC-01: Lisp Critic (Style)**
```lisp
(iiscv:make-atomic-commit
  '(defun test-style (x)
     "Should trigger IDIOMATIC-01 (use incf instead of setq)"
     (setq x (+ x 1))
     x))
```

---

### 2. Security & Performance Rules

**Rule 3.1: Unsafe External Execution**
```lisp
(iiscv:make-atomic-commit
  '(defun test-unsafe-shell ()
     "Should trigger Rule 3.1"
     (uiop:run-program "rm  test.md")))
```

**Rule 4.1: Consing in Critical Loop**
```lisp
(iiscv:make-atomic-commit
  '(defun test-heavy-consing ()
     "Should trigger Rule 4.1"
     (loop for i from 1 to 10
           do (print (list i i i)))))
```

**Rule 6.1: Implementation Specific Symbols**
```lisp
(iiscv:make-atomic-commit
  '(defun test-implementation-symbols ()
     "Should trigger Rule 6.1 (SB-EXT is specific to SBCL)"
     (print sb-ext:*gc-run-time*)))
```

---

### 3. Logic Audit Rules

**LOGIC-02: Dead Code (Constant Predicate)**
```lisp
(iiscv:make-atomic-commit
  '(defun test-dead-code ()
     "Should trigger LOGIC-02"
     (if t
         (print "Always")
         (print "Never reachable"))))
```

**LOGIC-01: Side-Effect Inconsistency**
```lisp
(iiscv:make-atomic-commit
  '(defun test-side-effect-waste (val)
     "Should trigger LOGIC-01 (Modifies state but returns NIL)"
     (setf *some-global* val)
     nil))
```

**LOGIC-03: Predicate Contract Failure**
```lisp
(iiscv:make-atomic-commit
  '(defun data-valid-p (x)
     "Should trigger LOGIC-03 (Ends in -P but only returns NIL)"
     (when x (print x))
     nil))
```

---

### 4. NASA JPL "Power of Ten" Rules

**NASA-01: Direct Recursion Prohibited**
```lisp
(iiscv:make-atomic-commit
  '(defun test-recursion (n)
     "Should trigger NASA-01"
     (if (<= n 0)
         0
         (test-recursion (1- n)))))
```

**NASA-02: Unbounded Loops**
```lisp
(iiscv:make-atomic-commit
  '(defun test-infinite-loop ()
     "Should trigger NASA-02 (Loop with no exit clause)"
     (loop (print "I never stop"))))
```

**NASA-05: Low Assertion Density**
```lisp
(iiscv:make-atomic-commit
  '(defun test-low-defense (a b c d e f)
     "Should trigger NASA-05 (Long function, 0 assertions)"
     (print a) (print b) (print c) (print d) (print e) (print f)
     (print 1) (print 2) (print 3) (print 4) (print 5) (print 6)))
```

---

### 5. Forensic Impact & Curation Rules

**Step A: Define and Curate a Stable function**
```lisp
(iiscv:make-atomic-commit '(defun base-service () "Stable service" (print "RUN")))
(iiscv:manual-human-commit "Milestone 1.0" '(base-service))
```

**Step B: Define a Dependent function**
```lisp
(iiscv:make-atomic-commit
  '(defun app-layer ()
     "Depends on base-service"
     (base-service)))
```

**Step C: Redefine the Base (Triggers IMPACT)**
```lisp
(iiscv:make-atomic-commit
  '(defun base-service () "Modified base" (print "NEW VERSION")))
;; Check output: Should show [LOGIC-IMPACT] for APP-LAYER
```

**Step D: Test Curation Leak (SAFETY-01)**
*Now, `BASE-SERVICE` is `:EXPERIMENTAL` again (from Step C). If we try to curate `APP-LAYER` now, it should trigger a leak error.*
```lisp
(iiscv:manual-human-commit "Dangerous Curate" '(app-layer))
;; Check output: Should show [SAFETY-01] Curation Leak.
```

---

### Verification Summary
After running all the above, run this to see the full forensic audit trail:
```lisp
(iiscv:audit-atomic-history)
```

**Every commit in that list should have its corresponding Rule IDs attached.** If something didn't trigger, it means a sensor in `lisa-rules-aux-fn.lisp` needs a slight calibration. How did the results look?

```lisp
CL-USER> (ql:quickload :iiscv)
To load "iiscv":
  Load 1 ASDF system:
    iiscv
; Loading "iiscv"
..................
(:IISCV)
CL-USER> (in-package :iiscv)
#<PACKAGE "IISCV">
IISCV> (iiscv-repl)

IISCV-R> (defun test-magic-numbers ()
     "Should trigger Rule 1.3"
     (+ 42 999))

[AUDIT] TEST-MAGIC-NUMBERS | Violations: 2
Symbol 'TEST-MAGIC-NUMBERS' is missing a docstring.
Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.

TEST-MAGIC-NUMBERS 
IISCV-R> ; Evaluation aborted on NIL.
IISCV> (iiscv:clear-all-commits)


[IISCV] All forensic history cleared.
NIL
IISCV> (iiscv:make-atomic-commit
  '(defun test-magic-numbers ()
     "Should trigger Rule 1.3"
     (+ 42 999)))

[AUDIT] TEST-MAGIC-NUMBERS | Violations: 2
Symbol 'TEST-MAGIC-NUMBERS' is missing a docstring.
Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.
"FB4A714F-DBF5-492A-8327-009E40E81DF5"
IISCV> (iiscv:make-atomic-commit
  '(defun test-no-docstring (x)
     (print x)))

[AUDIT] TEST-NO-DOCSTRING | Violations: 2
Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-NO-DOCSTRING' is missing a docstring.
"7FDE12EA-6DC6-4692-869F-9D4CD84B58B6"
IISCV> (iiscv:make-atomic-commit
  '(defun test-complex-and-long (a b c d e f g)
     "Should trigger Rule 1.1 (>10 complexity) and 1.2 (>25 lines)"
     (if a (if b (if c (if d (print 1) (print 2)))))
     (if e (if f (if g (print 3) (print 4))))
     (cond (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
     (print 1) (print 2) (print 3) (print 4) (print 5)
     (print 6) (print 7) (print 8) (print 9) (print 10)
     (print 11) (print 12) (print 13) (print 14) (print 15)
     (print 16) (print 17) (print 18) (print 19) (print 20)))

[AUDIT] TEST-COMPLEX-AND-LONG | Violations: 4
Low Assertion Density in 'TEST-COMPLEX-AND-LONG': No safety checks (assert/check-type) found.
Style recommendations for 'TEST-COMPLEX-AND-LONG': If the return value of a COND is being used, then be sure to have an
ELSE branch, i.e., (T ...). If it's not being used, use WHEN or
UNLESS.

Definition is  too long! A "little" is probably OK, "somewhat" might
be OK, if this is a really complicated problem, but code that is "too
long" or "way too long" can almost certainly be improved.

You have an IF with no else branch. If the return value of the IF
matters, you should explicitly say what the else returns, e.g., NIL.
If the return value doesn't matter, use WHEN or UNLESS.

Avoid nested IF's. Use AND, if possible, or a single COND.

In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-COMPLEX-AND-LONG' is missing a docstring.
Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.
"F45E3E3A-1C8C-4B32-A1A1-FB3CA4E21370"
IISCV> (iiscv:make-atomic-commit
  '(defun test-style (x)
     "Should trigger IDIOMATIC-01 (use incf instead of setq)"
     (setq x (+ x 1))
     x))

[AUDIT] TEST-STYLE | Violations: 2
Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.
Symbol 'TEST-STYLE' is missing a docstring.
"B82C043E-1082-418C-866F-36474C9232C1"
IISCV> (iiscv:make-atomic-commit
  '(defun test-unsafe-shell ()
     "Should trigger Rule 3.1"
     (uiop:run-program "rm  test.md")))

[AUDIT] TEST-UNSAFE-SHELL | Violations: 2
Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.
Symbol 'TEST-UNSAFE-SHELL' is missing a docstring.
"4A801EA8-65D3-4A89-B07C-F4E67E691A5A"
IISCV> (iiscv:make-atomic-commit
  '(defun test-heavy-consing ()
     "Should trigger Rule 4.1"
     (loop for i from 1 to 10
           do (print (list i i i)))))

[AUDIT] TEST-HEAVY-CONSING | Violations: 3
Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-HEAVY-CONSING' is missing a docstring.
Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.
"A8F25FF5-56C4-4E20-A581-6BADFDC151C7"
IISCV> (iiscv:make-atomic-commit
  '(defun test-implementation-symbols ()
     "Should trigger Rule 6.1 (SB-EXT is specific to SBCL)"
     (print sb-ext:*gc-run-time*)))

[AUDIT] TEST-IMPLEMENTATION-SYMBOLS | Violations: 2
Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-IMPLEMENTATION-SYMBOLS' is missing a docstring.
"1C8FF106-FB20-4000-A0CC-2FB6CAB1BF2C"
IISCV> (iiscv:make-atomic-commit
  '(defun test-dead-code ()
     "Should trigger LOGIC-02"
     (if t
         (print "Always")
         (print "Never reachable"))))

[AUDIT] TEST-DEAD-CODE | Violations: 3
Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.
Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-DEAD-CODE' is missing a docstring.
"8B3B14E4-AD16-4E67-9B43-1F2613B492D1"
IISCV> (iiscv:make-atomic-commit
  '(defun test-side-effect-waste (val)
     "Should trigger LOGIC-01 (Modifies state but returns NIL)"
     (setf *some-global* val)
     nil))

[AUDIT] TEST-SIDE-EFFECT-WASTE | Violations: 2
Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*
Symbol 'TEST-SIDE-EFFECT-WASTE' is missing a docstring.
"4FEC98D4-ED1E-42A3-93DD-0FEFCE9772AD"
IISCV> (iiscv:make-atomic-commit
  '(defun data-valid-p (x)
     "Should trigger LOGIC-03 (Ends in -P but only returns NIL)"
     (when x (print x))
     nil))

[AUDIT] DATA-VALID-P | Violations: 2
Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'DATA-VALID-P' is missing a docstring.
"DBC3B900-742A-4FC6-AE78-03D518065793"
IISCV> (iiscv:make-atomic-commit
  '(defun test-recursion (n)
     "Should trigger NASA-01"
     (if (<= n 0)
         0
         (test-recursion (1- n)))))
; in:
;      ASSERT (CODE-COMMIT-ANALYSIS
;          (COMMIT-UUID "23DEB492-4EE3-4AF9-A782-2ADD73AE4B84")
;          (SYMBOL-NAME TEST-RECURSION) (BODY-LENGTH 1) (CYCLOMATIC-COMPLEXITY 2)
;          (MAGIC-NUMBERS 'NIL) (IS-REDEFINING-CORE-SYMBOL-P NIL)
;          (USES-UNSAFE-EXECUTION-P NIL) (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
;          (HAS-DOCSTRING-P NIL) (UNUSED-PARAMETERS 'NIL)
;          (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL) ...)
;     ("IISCV::TEST-RECURSION")
; 
; caught ERROR:
;   illegal function call
; 
; compilation unit finished
;   caught 1 ERROR condition
; Evaluation aborted on #<SB-INT:COMPILED-PROGRAM-ERROR {100B63BE63}>.
IISCV> (iiscv:make-atomic-commit
  '(defun test-infinite-loop ()
     "Should trigger NASA-02 (Loop with no exit clause)"
     (loop (print "I never stop"))))

[AUDIT] TEST-INFINITE-LOOP | Violations: 3
Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.
Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-INFINITE-LOOP' is missing a docstring.
"5F78F4A2-BB4C-41B8-9E32-187CAC29DC73"
IISCV> (iiscv:make-atomic-commit
  '(defun test-low-defense (a b c d e f)
     "Should trigger NASA-05 (Long function, 0 assertions)"
     (print a) (print b) (print c) (print d) (print e) (print f)
     (print 1) (print 2) (print 3) (print 4) (print 5) (print 6)))

[AUDIT] TEST-LOW-DEFENSE | Violations: 4
Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.
Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-LOW-DEFENSE' is missing a docstring.
Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.
"B407339B-099C-4505-984D-B0831797E168"
IISCV> (iiscv:make-atomic-commit '(defun base-service () "Stable service" (print "RUN")))


[AUDIT] BASE-SERVICE | Violations: 2
Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'BASE-SERVICE' is missing a docstring.
"AC1D9ABF-81F9-4041-90D3-5502F9203A9F"
IISCV> (iiscv:manual-human-commit "Milestone 1.0" '(base-service))

[CURATION] BASE-SERVICE promoted to :CURATED.

[IISCV] Human commit created: Milestone 1.0
"48767067-EE12-4F49-91CD-4299440B08C9"
IISCV> (iiscv:make-atomic-commit
  '(defun app-layer ()
     "Depends on base-service"
     (base-service)))

[AUDIT] APP-LAYER | Violations: 1
Symbol 'APP-LAYER' is missing a docstring.
"3536D09C-F327-4A7E-A23E-A89C7782D981"
IISCV> (iiscv:make-atomic-commit
  '(defun base-service () "Modified base" (print "NEW VERSION")))

[AUDIT] BASE-SERVICE | Violations: 4
Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.
Mutation detected: 'BASE-SERVICE' has been updated in the history.
Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'BASE-SERVICE' is missing a docstring.
"7645524E-9CAF-4077-85B6-6A2A856EB651"
IISCV> (iiscv:manual-human-commit "Dangerous Curate" '(app-layer))

[CURATION] APP-LAYER promoted to :CURATED.

[IISCV] Human commit created: Dangerous Curate
"7028006E-BB75-4C40-960C-6FAE67179DC2"
IISCV> (iiscv:audit-atomic-history)

--- Atomic History Audit (Blockchain) ---

* Atomic Commit: FB4A714F-DBF5-492A-8327-009E40E81DF5
  Message: No docstring provided.
  Form: (DEFUN TEST-MAGIC-NUMBERS () Should trigger Rule 1.3 (+ 42 999))
  Timestamp: 3979138598
  Violations detected: (Symbol 'TEST-MAGIC-NUMBERS' is missing a docstring.
                        Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.)

* Atomic Commit: 7FDE12EA-6DC6-4692-869F-9D4CD84B58B6
  Message: No docstring provided.
  Form: (DEFUN TEST-NO-DOCSTRING (X) (PRINT X))
  Timestamp: 3979138609
  Violations detected: (Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-NO-DOCSTRING' is missing a docstring.)

* Atomic Commit: F45E3E3A-1C8C-4B32-A1A1-FB3CA4E21370
  Message: No docstring provided.
  Form: (DEFUN TEST-COMPLEX-AND-LONG (A B C D E F G)
          Should trigger Rule 1.1 (>10 complexity) and 1.2 (>25 lines)
          (IF A
              (IF B
                  (IF C
                      (IF D
                          (PRINT 1)
                          (PRINT 2)))))
          (IF E
              (IF F
                  (IF G
                      (PRINT 3)
                      (PRINT 4))))
          (COND (A 1) (B 2) (C 3) (D 4) (E 5) (F 6))
          (PRINT 1)
          (PRINT 2)
          (PRINT 3)
          (PRINT 4)
          (PRINT 5)
          (PRINT 6)
          (PRINT 7)
          (PRINT 8)
          (PRINT 9)
          (PRINT 10)
          (PRINT 11)
          (PRINT 12)
          (PRINT 13)
          (PRINT 14)
          (PRINT 15)
          (PRINT 16)
          (PRINT 17)
          (PRINT 18)
          (PRINT 19)
          (PRINT 20))
  Timestamp: 3979138636
  Violations detected: (Low Assertion Density in 'TEST-COMPLEX-AND-LONG': No safety checks (assert/check-type) found.
                        Style recommendations for 'TEST-COMPLEX-AND-LONG': If the return value of a COND is being used, then be sure to have an
ELSE branch, i.e., (T ...). If it's not being used, use WHEN or
UNLESS.

Definition is  too long! A "little" is probably OK, "somewhat" might
be OK, if this is a really complicated problem, but code that is "too
long" or "way too long" can almost certainly be improved.

You have an IF with no else branch. If the return value of the IF
matters, you should explicitly say what the else returns, e.g., NIL.
If the return value doesn't matter, use WHEN or UNLESS.

Avoid nested IF's. Use AND, if possible, or a single COND.

In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-COMPLEX-AND-LONG' is missing a docstring.
                        Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.)

* Atomic Commit: B82C043E-1082-418C-866F-36474C9232C1
  Message: No docstring provided.
  Form: (DEFUN TEST-STYLE (X)
          Should trigger IDIOMATIC-01 (use incf instead of setq)
          (SETQ X (+ X 1))
          X)
  Timestamp: 3979138680
  Violations detected: (Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.
                        Symbol 'TEST-STYLE' is missing a docstring.)

* Atomic Commit: 4A801EA8-65D3-4A89-B07C-F4E67E691A5A
  Message: No docstring provided.
  Form: (DEFUN TEST-UNSAFE-SHELL ()
          Should trigger Rule 3.1
          (RUN-PROGRAM rm  test.md))
  Timestamp: 3979138708
  Violations detected: (Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.
                        Symbol 'TEST-UNSAFE-SHELL' is missing a docstring.)

* Atomic Commit: A8F25FF5-56C4-4E20-A581-6BADFDC151C7
  Message: No docstring provided.
  Form: (DEFUN TEST-HEAVY-CONSING ()
          Should trigger Rule 4.1
          (LOOP FOR I FROM 1 TO 10
                DO (PRINT (LIST I I I))))
  Timestamp: 3979138723
  Violations detected: (Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-HEAVY-CONSING' is missing a docstring.
                        Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.)

* Atomic Commit: 1C8FF106-FB20-4000-A0CC-2FB6CAB1BF2C
  Message: No docstring provided.
  Form: (DEFUN TEST-IMPLEMENTATION-SYMBOLS ()
          Should trigger Rule 6.1 (SB-EXT is specific to SBCL)
          (PRINT *GC-RUN-TIME*))
  Timestamp: 3979138743
  Violations detected: (Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-IMPLEMENTATION-SYMBOLS' is missing a docstring.)

* Atomic Commit: 8B3B14E4-AD16-4E67-9B43-1F2613B492D1
  Message: No docstring provided.
  Form: (DEFUN TEST-DEAD-CODE ()
          Should trigger LOGIC-02
          (IF T
              (PRINT Always)
              (PRINT Never reachable)))
  Timestamp: 3979138762
  Violations detected: (Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.
                        Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-DEAD-CODE' is missing a docstring.)

* Atomic Commit: 4FEC98D4-ED1E-42A3-93DD-0FEFCE9772AD
  Message: No docstring provided.
  Form: (DEFUN TEST-SIDE-EFFECT-WASTE (VAL)
          Should trigger LOGIC-01 (Modifies state but returns NIL)
          (SETF *SOME-GLOBAL* VAL)
          NIL)
  Timestamp: 3979138784
  Violations detected: (Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*
                        Symbol 'TEST-SIDE-EFFECT-WASTE' is missing a docstring.)

* Atomic Commit: DBC3B900-742A-4FC6-AE78-03D518065793
  Message: No docstring provided.
  Form: (DEFUN DATA-VALID-P (X)
          Should trigger LOGIC-03 (Ends in -P but only returns NIL)
          (WHEN X (PRINT X))
          NIL)
  Timestamp: 3979138805
  Violations detected: (Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'DATA-VALID-P' is missing a docstring.)

* Atomic Commit: 5F78F4A2-BB4C-41B8-9E32-187CAC29DC73
  Message: No docstring provided.
  Form: (DEFUN TEST-INFINITE-LOOP ()
          Should trigger NASA-02 (Loop with no exit clause)
          (LOOP (PRINT I never stop)))
  Timestamp: 3979138897
  Violations detected: (Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.
                        Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-INFINITE-LOOP' is missing a docstring.)

* Atomic Commit: B407339B-099C-4505-984D-B0831797E168
  Message: No docstring provided.
  Form: (DEFUN TEST-LOW-DEFENSE (A B C D E F)
          Should trigger NASA-05 (Long function, 0 assertions)
          (PRINT A)
          (PRINT B)
          (PRINT C)
          (PRINT D)
          (PRINT E)
          (PRINT F)
          (PRINT 1)
          (PRINT 2)
          (PRINT 3)
          (PRINT 4)
          (PRINT 5)
          (PRINT 6))
  Timestamp: 3979138908
  Violations detected: (Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.
                        Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-LOW-DEFENSE' is missing a docstring.
                        Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.)

* Atomic Commit: AC1D9ABF-81F9-4041-90D3-5502F9203A9F
  Message: No docstring provided.
  Form: (DEFUN BASE-SERVICE () Stable service (PRINT RUN))
  Timestamp: 3979138915
  Violations detected: (Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'BASE-SERVICE' is missing a docstring.)

* Atomic Commit: 3536D09C-F327-4A7E-A23E-A89C7782D981
  Message: No docstring provided.
  Form: (DEFUN APP-LAYER () Depends on base-service (BASE-SERVICE))
  Timestamp: 3979138934
  Violations detected: (Symbol 'APP-LAYER' is missing a docstring.)

* Atomic Commit: 7645524E-9CAF-4077-85B6-6A2A856EB651
  Message: No docstring provided.
  Form: (DEFUN BASE-SERVICE () Modified base (PRINT NEW VERSION))
  Timestamp: 3979138956
  Violations detected: (Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.
                        Mutation detected: 'BASE-SERVICE' has been updated in the history.
                        Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'BASE-SERVICE' is missing a docstring.)
NIL
```
