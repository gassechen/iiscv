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
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL) (STYLE-CRITIQUES NIL)
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS ((999 42))) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-MAGIC-NUMBERS)
         (COMMIT-UUID "22368305-8B37-4AAA-ADD7-9277D3675CEC"))
==> Activation: RULE-1-3-MAGIC-NUMBER-USAGE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-1-3-MAGIC-NUMBER-USAGE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-MAGIC-NUMBERS) (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))

[AUDIT] TEST-MAGIC-NUMBERS | Violations: 1
Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.
"22368305-8B37-4AAA-ADD7-9277D3675CEC"	 
	 
```

**Rule 5.1: Missing Docstring**
```lisp
(iiscv:make-atomic-commit
  '(defun test-no-docstring (x)
     (print x)))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P NIL)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-NO-DOCSTRING)
         (COMMIT-UUID "8A67AEAC-CA5F-4AB1-9079-1AABF5972780"))
==> Activation: RULE-5-1-MISSING-DOCSTRING : (F-1)
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-5-1-MISSING-DOCSTRING (F-1)
==> F-2 (VIOLATION
         (MESSAGE "Symbol 'TEST-NO-DOCSTRING' is missing a docstring.")
         (SEVERITY :INFO) (RULE-ID "5.1"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-4 (GOAL (STATUS ACTIVE) (TARGET TEST-NO-DOCSTRING) (TYPE VALIDATE-LOGIC))
FIRE 4: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE "Symbol 'TEST-NO-DOCSTRING' is missing a docstring.")
         (SEVERITY :INFO) (RULE-ID "5.1"))
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] TEST-NO-DOCSTRING | Violations: 2
Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.
Symbol 'TEST-NO-DOCSTRING' is missing a docstring.
"8A67AEAC-CA5F-4AB1-9079-1AABF5972780"	 
	 
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
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "If the return value of a COND is being used, then be sure to have an
ELSE branch, i.e., (T ...). If it's not being used, use WHEN or
UNLESS.

Definition is  too long! A \"little\" is probably OK, \"somewhat\" might
be OK, if this is a really complicated problem, but code that is \"too
long\" or \"way too long\" can almost certainly be improved.

You have an IF with no else branch. If the return value of the IF
matters, you should explicitly say what the else returns, e.g., NIL.
If the return value doesn't matter, use WHEN or UNLESS.

Avoid nested IF's. Use AND, if possible, or a single COND.

In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 9)
         (UNUSED-PARAMETERS NIL)
         (MAGIC-NUMBERS ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)))
         (HAS-DOCSTRING-P T) (BODY-LENGTH 23) (SYMBOL-TYPE NIL)
         (SYMBOL-NAME TEST-COMPLEX-AND-LONG)
         (COMMIT-UUID "15BEBBA1-7FA0-4C4A-9155-C312040D2F06"))
==> Activation: RULE-1-3-MAGIC-NUMBER-USAGE : (F-1)
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-1-3-MAGIC-NUMBER-USAGE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-COMPLEX-AND-LONG': If the return value of a COND is being used, then be sure to have an
ELSE branch, i.e., (T ...). If it's not being used, use WHEN or
UNLESS.

Definition is  too long! A \"little\" is probably OK, \"somewhat\" might
be OK, if this is a really complicated problem, but code that is \"too
long\" or \"way too long\" can almost certainly be improved.

You have an IF with no else branch. If the return value of the IF
matters, you should explicitly say what the else returns, e.g., NIL.
If the return value doesn't matter, use WHEN or UNLESS.

Avoid nested IF's. Use AND, if possible, or a single COND.

In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-4 (GOAL (STATUS ACTIVE) (TARGET TEST-COMPLEX-AND-LONG)
         (TYPE VALIDATE-LOGIC))
==> Activation: RULE-NASA-05-ASSERTION-DENSITY : (F-4 F-1)
FIRE 4: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-COMPLEX-AND-LONG': If the return value of a COND is being used, then be sure to have an
ELSE branch, i.e., (T ...). If it's not being used, use WHEN or
UNLESS.

Definition is  too long! A \"little\" is probably OK, \"somewhat\" might
be OK, if this is a really complicated problem, but code that is \"too
long\" or \"way too long\" can almost certainly be improved.

You have an IF with no else branch. If the return value of the IF
matters, you should explicitly say what the else returns, e.g., NIL.
If the return value doesn't matter, use WHEN or UNLESS.

Avoid nested IF's. Use AND, if possible, or a single COND.

In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
FIRE 6: RULE-NASA-05-ASSERTION-DENSITY (F-4 F-1)
==> F-5 (VIOLATION
         (MESSAGE
          "Low Assertion Density in 'TEST-COMPLEX-AND-LONG': No safety checks (assert/check-type) found.")
         (SEVERITY :WARNING) (RULE-ID "NASA-05"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-5)
FIRE 7: RULE-BRIDGE-VIOLATIONS (F-5)
<== F-5 (VIOLATION
         (MESSAGE
          "Low Assertion Density in 'TEST-COMPLEX-AND-LONG': No safety checks (assert/check-type) found.")
         (SEVERITY :WARNING) (RULE-ID "NASA-05"))

[AUDIT] TEST-COMPLEX-AND-LONG | Violations: 3
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
Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.
"15BEBBA1-7FA0-4C4A-9155-C312040D2F06"	 
	 
```

**Rule IDIOMATIC-01: Lisp Critic (Style)**
```lisp
(iiscv:make-atomic-commit
  '(defun test-style (x)
     "Should trigger IDIOMATIC-01 (use incf instead of setq)"
     (setq x (+ x 1))
     x))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P T) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES "INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 2) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-STYLE)
         (COMMIT-UUID "97421742-3FBC-46CA-B191-55BA999672D1"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-STYLE) (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] TEST-STYLE | Violations: 1
Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.
"97421742-3FBC-46CA-B191-55BA999672D1"	 
```

---

### 2. Security & Performance Rules

**Rule 3.1: Unsafe External Execution**
```lisp
(iiscv:make-atomic-commit
  '(defun test-unsafe-shell ()
     "Should trigger Rule 3.1"
     (uiop:run-program "rm  test.md")))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL) (STYLE-CRITIQUES NIL)
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P T) (USES-UNSAFE-EXECUTION-P T)
         (CYCLOMATIC-COMPLEXITY 1) (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL)
         (HAS-DOCSTRING-P T) (BODY-LENGTH 1) (SYMBOL-TYPE NIL)
         (SYMBOL-NAME TEST-UNSAFE-SHELL)
         (COMMIT-UUID "D78C912F-7D6C-4BCE-975C-964EE954FD50"))
==> Activation: RULE-3-1-UNSAFE-COMMAND-EXECUTION : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-3-1-UNSAFE-COMMAND-EXECUTION (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.")
         (SEVERITY :ERROR) (RULE-ID "3.1"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-UNSAFE-SHELL) (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.")
         (SEVERITY :ERROR) (RULE-ID "3.1"))

[AUDIT] TEST-UNSAFE-SHELL | Violations: 1
Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.
"D78C912F-7D6C-4BCE-975C-964EE954FD50"	 
```

**Rule 4.1: Consing in Critical Loop**
```lisp
(iiscv:make-atomic-commit
  '(defun test-heavy-consing ()
     "Should trigger Rule 4.1"
     (loop for i from 1 to 10
           do (print (list i i i)))))
		   

==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P T)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 2)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS ((10))) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-HEAVY-CONSING)
         (COMMIT-UUID "353EB98F-E0E6-4E53-9BFD-7CA9F5D61EE4"))
==> Activation: RULE-1-3-MAGIC-NUMBER-USAGE : (F-1)
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-1-3-MAGIC-NUMBER-USAGE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-4 (GOAL (STATUS ACTIVE) (TARGET TEST-HEAVY-CONSING) (TYPE VALIDATE-LOGIC))
FIRE 4: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] TEST-HEAVY-CONSING | Violations: 2
Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.
Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.
"353EB98F-E0E6-4E53-9BFD-7CA9F5D61EE4"

```

**Rule 6.1: Implementation Specific Symbols**
```lisp
(iiscv:make-atomic-commit
  '(defun test-implementation-symbols ()
     "Should trigger Rule 6.1 (SB-EXT is specific to SBCL)"
     (print sb-ext:*gc-run-time*)))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P T)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL)
         (SYMBOL-NAME TEST-IMPLEMENTATION-SYMBOLS)
         (COMMIT-UUID "BA695860-FBC7-49B6-B4D4-B19C45AE89C4"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-IMPLEMENTATION-SYMBOLS)
         (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] TEST-IMPLEMENTATION-SYMBOLS | Violations: 1
Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.
"BA695860-FBC7-49B6-B4D4-B19C45AE89C4"

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
		 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P T) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 2)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-DEAD-CODE)
         (COMMIT-UUID "F957517E-DD52-4549-A025-28F87071ABD3"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-DEAD-CODE) (TYPE VALIDATE-LOGIC))
==> Activation: RULE-LOGIC-UNREACHABLE-CODE : (F-3 F-1)
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
FIRE 4: RULE-LOGIC-UNREACHABLE-CODE (F-3 F-1)
==> F-4 (VIOLATION
         (MESSAGE
          "Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.")
         (SEVERITY :ERROR) (RULE-ID "LOGIC-02"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-4)
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-4)
<== F-4 (VIOLATION
         (MESSAGE
          "Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.")
         (SEVERITY :ERROR) (RULE-ID "LOGIC-02"))

[AUDIT] TEST-DEAD-CODE | Violations: 2
Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.
Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.
"F957517E-DD52-4549-A025-28F87071ABD3"		 
```

**LOGIC-01: Side-Effect Inconsistency**
```lisp
(iiscv:make-atomic-commit
  '(defun test-side-effect-waste (val)
     "Should trigger LOGIC-01 (Modifies state but returns NIL)"
     (setf *some-global* val)
     nil))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P T)
         (HAS-SIDE-EFFECTS-P T) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 2) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-SIDE-EFFECT-WASTE)
         (COMMIT-UUID "0C34E922-3B24-4A94-A4F9-CF572DB022C0"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-SIDE-EFFECT-WASTE)
         (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] TEST-SIDE-EFFECT-WASTE | Violations: 1
Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*
"0C34E922-3B24-4A94-A4F9-CF572DB022C0"	 
```

**LOGIC-03: Predicate Contract Failure**
```lisp
(iiscv:make-atomic-commit
  '(defun data-valid-p (x)
     "Should trigger LOGIC-03 (Ends in -P but only returns NIL)"
     (when x (print x))
     nil))
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P T) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P T)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 2)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 2) (SYMBOL-TYPE NIL) (SYMBOL-NAME DATA-VALID-P)
         (COMMIT-UUID "800493DD-67F2-482F-92C7-B85A3DA5160E"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET DATA-VALID-P) (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] DATA-VALID-P | Violations: 1
Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.
"800493DD-67F2-482F-92C7-B85A3DA5160E"	 
	 
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

==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P T) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL)
         (CALLS ("IISCV::TEST-RECURSION")) (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES NIL) (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 2)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-RECURSION)
         (COMMIT-UUID "9BFD7996-CB21-4543-81D1-E3EE7710B192"))
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-2 (GOAL (STATUS ACTIVE) (TARGET TEST-RECURSION) (TYPE VALIDATE-LOGIC))
==> Activation: RULE-NASA-01-NO-RECURSION : (F-2 F-1)
FIRE 2: RULE-NASA-01-NO-RECURSION (F-2 F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Recursion Violation: 'TEST-RECURSION' calls itself. Prohibited in high-integrity code.")
         (SEVERITY :WARNING) (RULE-ID "NASA-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Recursion Violation: 'TEST-RECURSION' calls itself. Prohibited in high-integrity code.")
         (SEVERITY :WARNING) (RULE-ID "NASA-01"))

[AUDIT] TEST-RECURSION | Violations: 1
Recursion Violation: 'TEST-RECURSION' calls itself. Prohibited in high-integrity code.
"9BFD7996-CB21-4543-81D1-E3EE7710B192"
```

**NASA-02: Unbounded Loops**
```lisp
(iiscv:make-atomic-commit
  '(defun test-infinite-loop ()
     "Should trigger NASA-02 (Loop with no exit clause)"
     (loop (print "I never stop"))))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P T)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 2)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME TEST-INFINITE-LOOP)
         (COMMIT-UUID "7243CE43-2660-48D6-8F2B-81EC5ED7B1FA"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET TEST-INFINITE-LOOP) (TYPE VALIDATE-LOGIC))
==> Activation: RULE-NASA-02-UNBOUNDED-LOOP : (F-3 F-1)
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
FIRE 4: RULE-NASA-02-UNBOUNDED-LOOP (F-3 F-1)
==> F-4 (VIOLATION
         (MESSAGE
          "Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.")
         (SEVERITY :ERROR) (RULE-ID "NASA-02"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-4)
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-4)
<== F-4 (VIOLATION
         (MESSAGE
          "Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.")
         (SEVERITY :ERROR) (RULE-ID "NASA-02"))

[AUDIT] TEST-INFINITE-LOOP | Violations: 2
Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.
Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.
"7243CE43-2660-48D6-8F2B-81EC5ED7B1FA"	 
	 
```

**NASA-05: Low Assertion Density**
```lisp
(iiscv:make-atomic-commit
  '(defun test-low-defense (a b c d e f)
     "Should trigger NASA-05 (Long function, 0 assertions)"
     (print a) (print b) (print c) (print d) (print e) (print f)
     (print 1) (print 2) (print 3) (print 4) (print 5) (print 6)))
	 
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS ((6 5 4 3 2)))
         (HAS-DOCSTRING-P T) (BODY-LENGTH 12) (SYMBOL-TYPE NIL)
         (SYMBOL-NAME TEST-LOW-DEFENSE)
         (COMMIT-UUID "FDBAEB9C-9014-4FEA-90B4-1C82297C465D"))
==> Activation: RULE-1-3-MAGIC-NUMBER-USAGE : (F-1)
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-1-3-MAGIC-NUMBER-USAGE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-4 (GOAL (STATUS ACTIVE) (TARGET TEST-LOW-DEFENSE) (TYPE VALIDATE-LOGIC))
==> Activation: RULE-NASA-05-ASSERTION-DENSITY : (F-4 F-1)
FIRE 4: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.")
         (SEVERITY :WARNING) (RULE-ID "1.3"))
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
FIRE 6: RULE-NASA-05-ASSERTION-DENSITY (F-4 F-1)
==> F-5 (VIOLATION
         (MESSAGE
          "Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.")
         (SEVERITY :WARNING) (RULE-ID "NASA-05"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-5)
FIRE 7: RULE-BRIDGE-VIOLATIONS (F-5)
<== F-5 (VIOLATION
         (MESSAGE
          "Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.")
         (SEVERITY :WARNING) (RULE-ID "NASA-05"))

[AUDIT] TEST-LOW-DEFENSE | Violations: 3
Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.
Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.
Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.
"FDBAEB9C-9014-4FEA-90B4-1C82297C465D"	 
```

---

### 5. Forensic Impact & Curation Rules

**Step A: Define and Curate a Stable function**
```lisp
(iiscv:make-atomic-commit '(defun base-service () "Stable service" (print "RUN")))
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME BASE-SERVICE)
         (COMMIT-UUID "082C07F9-795F-4112-A217-19BDDE3A95F0"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-3 (GOAL (STATUS ACTIVE) (TARGET BASE-SERVICE) (TYPE VALIDATE-LOGIC))
FIRE 3: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))

[AUDIT] BASE-SERVICE | Violations: 1
Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
"082C07F9-795F-4112-A217-19BDDE3A95F0"


(iiscv:manual-human-commit "Milestone 1.0" '(base-service))

[CURATION] BASE-SERVICE promoted to :CURATED.

[IISCV] Human commit created: Milestone 1.0
"07ABD9F9-65DB-424D-9D49-F32615545474"
```

**Step B: Define a Dependent function**
```lisp
(iiscv:make-atomic-commit
  '(defun app-layer ()
     "Depends on base-service"
     (base-service)))
==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL)
         (CALLS ("IISCV::BASE-SERVICE")) (IS-REDEFINING-CORE-SYMBOL-P NIL)
         (STYLE-CRITIQUES NIL) (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME APP-LAYER)
         (COMMIT-UUID "9DBE3FF5-7BF5-4970-BDCA-89E8D7C3A1DB"))
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
FIRE 1: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-2 (GOAL (STATUS ACTIVE) (TARGET APP-LAYER) (TYPE VALIDATE-LOGIC))

[AUDIT] APP-LAYER | Violations: 0
"9DBE3FF5-7BF5-4970-BDCA-89E8D7C3A1DB"	 
	 
```

**Step C: Redefine the Base (Triggers IMPACT)**
```lisp
(iiscv:make-atomic-commit
  '(defun base-service () "Modified base" (print "NEW VERSION")))
;; Check output: Should show [LOGIC-IMPACT] for APP-LAYER

==> F-0 (INITIAL-FACT)
==> F-0 (INITIAL-FACT)
==> F-1 (CODE-COMMIT-ANALYSIS (ASSERTION-COUNT 0) (HAS-UNBOUNDED-LOOP-P NIL)
         (IS-RECURSIVE-P NIL) (IS-PREDICATE-P NIL) (MUTATED-SYMBOLS NIL)
         (HAS-DEAD-CODE-P NIL) (RETURNS-CONSTANT-NIL-P NIL)
         (HAS-SIDE-EFFECTS-P NIL) (STATUS :EXPERIMENTAL) (CALLS NIL)
         (IS-REDEFINING-CORE-SYMBOL-P T)
         (STYLE-CRITIQUES
          "In general, FORMAT is used for most printing, because it's more
flexible.")
         (CONTAINS-HEAVY-CONSING-LOOP-P NIL)
         (USES-IMPLEMENTATION-SPECIFIC-SYMBOLS-P NIL)
         (USES-UNSAFE-EXECUTION-P NIL) (CYCLOMATIC-COMPLEXITY 1)
         (UNUSED-PARAMETERS NIL) (MAGIC-NUMBERS NIL) (HAS-DOCSTRING-P T)
         (BODY-LENGTH 1) (SYMBOL-TYPE NIL) (SYMBOL-NAME BASE-SERVICE)
         (COMMIT-UUID "701AB156-301D-4903-A57A-ADE0ECF26A73"))
==> Activation: RULE-IDIOMATIC-LISP-STYLE : (F-1)
==> Activation: RULE-2-2-INTERNAL-REDEFINITION : (F-1)
==> Activation: RULE-TRIGGER-LOGICAL-AUDIT : (F-1)
==> Activation: RULE-TRIGGER-IMPACT : (F-1)
FIRE 1: RULE-IDIOMATIC-LISP-STYLE (F-1)
==> F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-2)
FIRE 2: RULE-2-2-INTERNAL-REDEFINITION (F-1)
==> F-3 (VIOLATION
         (MESSAGE
          "Mutation detected: 'BASE-SERVICE' has been updated in the history.")
         (SEVERITY :INFO) (RULE-ID "2.2"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-3)
FIRE 3: RULE-TRIGGER-LOGICAL-AUDIT (F-1)
==> F-4 (GOAL (STATUS ACTIVE) (TARGET BASE-SERVICE) (TYPE VALIDATE-LOGIC))
FIRE 4: RULE-TRIGGER-IMPACT (F-1)
==> F-5 (GOAL (STATUS ACTIVE) (TARGET BASE-SERVICE) (TYPE TRACE-IMPACT))
==> Activation: RULE-PROCESS-IMPACT : (F-5)
FIRE 5: RULE-BRIDGE-VIOLATIONS (F-2)
<== F-2 (VIOLATION
         (MESSAGE
          "Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.")
         (SEVERITY :WARNING) (RULE-ID "IDIOMATIC-01"))
FIRE 6: RULE-BRIDGE-VIOLATIONS (F-3)
<== F-3 (VIOLATION
         (MESSAGE
          "Mutation detected: 'BASE-SERVICE' has been updated in the history.")
         (SEVERITY :INFO) (RULE-ID "2.2"))
FIRE 7: RULE-PROCESS-IMPACT (F-5)
==> F-6 (VIOLATION
         (MESSAGE
          "Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.")
         (SEVERITY :WARNING) (RULE-ID "LOGIC-IMPACT"))
==> Activation: RULE-BRIDGE-VIOLATIONS : (F-6)
<== F-5 (GOAL (STATUS ACTIVE) (TARGET BASE-SERVICE) (TYPE TRACE-IMPACT))
FIRE 8: RULE-BRIDGE-VIOLATIONS (F-6)
<== F-6 (VIOLATION
         (MESSAGE
          "Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.")
         (SEVERITY :WARNING) (RULE-ID "LOGIC-IMPACT"))

[AUDIT] BASE-SERVICE | Violations: 3
Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.
Mutation detected: 'BASE-SERVICE' has been updated in the history.
Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.
"701AB156-301D-4903-A57A-ADE0ECF26A73"

```

**Step D: Test Curation Leak (SAFETY-01)**
*Now, `BASE-SERVICE` is `:EXPERIMENTAL` again (from Step C). If we try to curate `APP-LAYER` now, it should trigger a leak error.*
```lisp
(iiscv:manual-human-commit "Dangerous Curate" '(app-layer))
;; Check output: Should show [SAFETY-01] Curation Leak.

[CURATION] APP-LAYER promoted to :CURATED.

[IISCV] Human commit created: Dangerous Curate
"F90F54AF-8305-48E6-8767-56DD0BAB4144"

```

---

### Verification Summary
After running all the above, run this to see the full forensic audit trail:
```lisp
(iiscv:audit-atomic-history)


--- Atomic History Audit (Blockchain) ---

* Atomic Commit: 22368305-8B37-4AAA-ADD7-9277D3675CEC
  Message: Should trigger Rule 1.3
  Form: (DEFUN TEST-MAGIC-NUMBERS () Should trigger Rule 1.3 (+ 42 999))
  Timestamp: 3979142529
  Violations detected: (Magic numbers ((999 42)) found in 'TEST-MAGIC-NUMBERS'. Define them as constants.)

* Atomic Commit: 8A67AEAC-CA5F-4AB1-9079-1AABF5972780
  Message: No docstring provided.
  Form: (DEFUN TEST-NO-DOCSTRING (X) (PRINT X))
  Timestamp: 3979142608
  Violations detected: (Style recommendations for 'TEST-NO-DOCSTRING': In general, FORMAT is used for most printing, because it's more
flexible.
                        Symbol 'TEST-NO-DOCSTRING' is missing a docstring.)

* Atomic Commit: 15BEBBA1-7FA0-4C4A-9155-C312040D2F06
  Message: Should trigger Rule 1.1 (>10 complexity) and 1.2 (>25 lines)
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
  Timestamp: 3979142641
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
                        Magic numbers ((20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)) found in 'TEST-COMPLEX-AND-LONG'. Define them as constants.)

* Atomic Commit: 97421742-3FBC-46CA-B191-55BA999672D1
  Message: Should trigger IDIOMATIC-01 (use incf instead of setq)
  Form: (DEFUN TEST-STYLE (X)
          Should trigger IDIOMATIC-01 (use incf instead of setq)
          (SETQ X (+ X 1))
          X)
  Timestamp: 3979142700
  Violations detected: (Style recommendations for 'TEST-STYLE': INCF would be simpler to add 1 to X than SETQ

It's bad style to reassign input parameters like X -- and often
useless.

Don't use (+ X 1), use (1+ X) for its value or (INCF X) to change X,
whichever is appropriate here.)

* Atomic Commit: D78C912F-7D6C-4BCE-975C-964EE954FD50
  Message: Should trigger Rule 3.1
  Form: (DEFUN TEST-UNSAFE-SHELL ()
          Should trigger Rule 3.1
          (RUN-PROGRAM rm  test.md))
  Timestamp: 3979142734
  Violations detected: (Unsafe command execution in 'TEST-UNSAFE-SHELL'. Sanitize all inputs.)

* Atomic Commit: 353EB98F-E0E6-4E53-9BFD-7CA9F5D61EE4
  Message: Should trigger Rule 4.1
  Form: (DEFUN TEST-HEAVY-CONSING ()
          Should trigger Rule 4.1
          (LOOP FOR I FROM 1 TO 10
                DO (PRINT (LIST I I I))))
  Timestamp: 3979142768
  Violations detected: (Style recommendations for 'TEST-HEAVY-CONSING': In general, FORMAT is used for most printing, because it's more
flexible.
                        Magic numbers ((10)) found in 'TEST-HEAVY-CONSING'. Define them as constants.)

* Atomic Commit: BA695860-FBC7-49B6-B4D4-B19C45AE89C4
  Message: Should trigger Rule 6.1 (SB-EXT is specific to SBCL)
  Form: (DEFUN TEST-IMPLEMENTATION-SYMBOLS ()
          Should trigger Rule 6.1 (SB-EXT is specific to SBCL)
          (PRINT *GC-RUN-TIME*))
  Timestamp: 3979142799
  Violations detected: (Style recommendations for 'TEST-IMPLEMENTATION-SYMBOLS': In general, FORMAT is used for most printing, because it's more
flexible.)

* Atomic Commit: F957517E-DD52-4549-A025-28F87071ABD3
  Message: Should trigger LOGIC-02
  Form: (DEFUN TEST-DEAD-CODE ()
          Should trigger LOGIC-02
          (IF T
              (PRINT Always)
              (PRINT Never reachable)))
  Timestamp: 3979142830
  Violations detected: (Dead Code in 'TEST-DEAD-CODE': Unreachable branches detected.
                        Style recommendations for 'TEST-DEAD-CODE': In general, FORMAT is used for most printing, because it's more
flexible.)

* Atomic Commit: 0C34E922-3B24-4A94-A4F9-CF572DB022C0
  Message: Should trigger LOGIC-01 (Modifies state but returns NIL)
  Form: (DEFUN TEST-SIDE-EFFECT-WASTE (VAL)
          Should trigger LOGIC-01 (Modifies state but returns NIL)
          (SETF *SOME-GLOBAL* VAL)
          NIL)
  Timestamp: 3979142868
  Violations detected: (Style recommendations for 'TEST-SIDE-EFFECT-WASTE': GLOBALS!! Don't use global variables, i.e., *SOME-GLOBAL*)

* Atomic Commit: 800493DD-67F2-482F-92C7-B85A3DA5160E
  Message: Should trigger LOGIC-03 (Ends in -P but only returns NIL)
  Form: (DEFUN DATA-VALID-P (X)
          Should trigger LOGIC-03 (Ends in -P but only returns NIL)
          (WHEN X (PRINT X))
          NIL)
  Timestamp: 3979142896
  Violations detected: (Style recommendations for 'DATA-VALID-P': In general, FORMAT is used for most printing, because it's more
flexible.)

* Atomic Commit: 9BFD7996-CB21-4543-81D1-E3EE7710B192
  Message: Should trigger NASA-01
  Form: (DEFUN TEST-RECURSION (N)
          Should trigger NASA-01
          (IF (<= N 0)
              0
              (TEST-RECURSION (1- N))))
  Timestamp: 3979142926
  Violations detected: (Recursion Violation: 'TEST-RECURSION' calls itself. Prohibited in high-integrity code.)

* Atomic Commit: 7243CE43-2660-48D6-8F2B-81EC5ED7B1FA
  Message: Should trigger NASA-02 (Loop with no exit clause)
  Form: (DEFUN TEST-INFINITE-LOOP ()
          Should trigger NASA-02 (Loop with no exit clause)
          (LOOP (PRINT I never stop)))
  Timestamp: 3979142960
  Violations detected: (Unbounded Loop in 'TEST-INFINITE-LOOP': All loops must have an exit clause.
                        Style recommendations for 'TEST-INFINITE-LOOP': In general, FORMAT is used for most printing, because it's more
flexible.)

* Atomic Commit: FDBAEB9C-9014-4FEA-90B4-1C82297C465D
  Message: Should trigger NASA-05 (Long function, 0 assertions)
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
  Timestamp: 3979143004
  Violations detected: (Low Assertion Density in 'TEST-LOW-DEFENSE': No safety checks (assert/check-type) found.
                        Style recommendations for 'TEST-LOW-DEFENSE': In general, FORMAT is used for most printing, because it's more
flexible.
                        Magic numbers ((6 5 4 3 2)) found in 'TEST-LOW-DEFENSE'. Define them as constants.)

* Atomic Commit: 082C07F9-795F-4112-A217-19BDDE3A95F0
  Message: Stable service
  Form: (DEFUN BASE-SERVICE () Stable service (PRINT RUN))
  Timestamp: 3979143043
  Violations detected: (Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.)

* Atomic Commit: 9DBE3FF5-7BF5-4970-BDCA-89E8D7C3A1DB
  Message: Depends on base-service
  Form: (DEFUN APP-LAYER () Depends on base-service (BASE-SERVICE))
  Timestamp: 3979143122
  Violations detected: NIL

* Atomic Commit: 701AB156-301D-4903-A57A-ADE0ECF26A73
  Message: Modified base
  Form: (DEFUN BASE-SERVICE () Modified base (PRINT NEW VERSION))
  Timestamp: 3979143152
  Violations detected: (Forensic Impact: 'APP-LAYER' depends on 'BASE-SERVICE' and requires review.
                        Mutation detected: 'BASE-SERVICE' has been updated in the history.
                        Style recommendations for 'BASE-SERVICE': In general, FORMAT is used for most printing, because it's more
flexible.)

```

**Every commit in that list should have its corresponding Rule IDs attached.** If something didn't trigger, it means a sensor in `lisa-rules-aux-fn.lisp` needs a slight calibration. How did the results look?

```lisp

(iiscv:show-project-milestones)

--- Project Milestones (Human History) ---

* Milestone: Milestone 1.0
  UUID: 07ABD9F9-65DB-424D-9D49-F32615545474
  Timestamp: 3979143077
  Atomic Changes: (082C07F9-795F-4112-A217-19BDDE3A95F0)

* Milestone: Dangerous Curate
  UUID: F90F54AF-8305-48E6-8767-56DD0BAB4144
  Timestamp: 3979143191
  Atomic Changes: (9DBE3FF5-7BF5-4970-BDCA-89E8D7C3A1DB)
--------------------------------------------
NIL
```
