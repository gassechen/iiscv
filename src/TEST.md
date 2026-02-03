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
     (uiop:run-program "rm -rf /")))
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
