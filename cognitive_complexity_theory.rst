############################
Cognitive Complexity Theory
############################

Introduction
============

Cognitive Complexity is a metric designed to measure the relative difficulty of
understanding code's control flow. Unlike Cyclomatic Complexity, which is based
on a mathematical model of the number of linearly independent paths through code,
Cognitive Complexity uses human judgment to assess the mental effort required to
understand control flow structures.

This implementation follows the **Cognitive Complexity specification version 1.2**
by SonarSource (19 April 2017). The full specification is available at:
https://www.sonarsource.com/docs/CognitiveComplexity.pdf

The key insight is that not all control flow structures impose equal mental burden.
For example, a ``switch`` statement with five cases is easier to understand than
a chain of five ``if-else if`` statements testing different variables, even though
both create the same number of execution paths. Cognitive Complexity captures this
intuition through its scoring rules.

Cognitive Complexity vs Cyclomatic Complexity
----------------------------------------------

**Cyclomatic Complexity** measures:

- Testability: minimum number of test cases for full coverage
- Based on graph theory (nodes and edges in control flow graph)
- Every method starts with CCN = 1
- Each decision point adds exactly +1

**Cognitive Complexity** measures:

- Maintainability: relative difficulty of understanding control flow
- Based on programmer intuition about mental effort
- Methods start with CogC = 0
- Different structures have different scoring (some ignored, some with nesting penalties)
- Aggregates meaningfully at class and application levels

Key differences:

1. **Switch statements**: CCN counts each case (+N), CogC counts switch once (+1)
2. **Nesting**: CCN ignores nesting, CogC adds nesting penalties
3. **Binary operators**: CCN counts each operator, CogC counts sequences
4. **Shorthand**: CogC ignores null-coalescing and similar readability aids
5. **Try blocks**: CogC excludes them from nesting calculations

Basic Calculation Rules
========================

Cognitive Complexity is calculated according to three basic rules:

1. **Ignore structures that allow multiple statements to be readably shorthanded into one**

   - Method definitions (top-level functions)
   - Null-coalescing operators (``?.``, ``??``)
   - Lambda expressions (aggregate to parent function)

2. **Increment (add one) for each break in the linear flow of the code**

   - Control structures: ``if``, ``for``, ``while``, ``switch``
   - Exception handling: ``catch``
   - Jump statements: ``goto``, labeled ``break``/``continue``
   - Logical operator sequences: ``&&``, ``||``

3. **Increment when flow-breaking structures are nested**

   - Nesting level multiplies the mental effort
   - Each level of nesting adds to the increment
   - Not all structures increase nesting (e.g., ``try`` blocks)

Four Types of Increments
-------------------------

While each increment adds one (or more) to the final score, understanding the
categories helps clarify where nesting penalties apply:

**A. Nesting Increments**

- Assessed for each level of nesting
- Added to structural and hybrid increments
- Formula: ``+1 + nesting_level``

**B. Structural Increments**

- Flow-breaking structures that ARE subject to nesting
- Also increase the nesting level for their children
- Keywords: ``if``, ``for``, ``while``, ``foreach``, ``#if``, ``#ifdef``, ``#elif``
- Scoring: ``+1`` at nesting 0, ``+2`` at nesting 1, ``+3`` at nesting 2, etc.

**C. Fundamental Increments**

- Flow-breaking structures NOT subject to nesting
- Do NOT increase nesting level
- Structures: binary logical sequences, ``goto``, labeled jumps, recursion
- Scoring: Always ``+1`` regardless of nesting

**D. Hybrid Increments**

- Structures that get ``+1`` without nesting penalty
- BUT do increase nesting level for their children
- Keywords: ``else``, ``elif``, ``elseif``
- Rationale: Mental cost already paid when reading the ``if``

Structural Increments
=====================

These are the primary control flow structures. Each adds ``+1`` plus the current
nesting level to Cognitive Complexity, and increases the nesting level for code
inside them.

Conditionals
------------

**Keywords**: ``if``, ``#if``, ``#ifdef``

.. code-block:: cpp

    void example() {
        if (condition1) {           // +1 (nesting=0)
            if (condition2) {       // +2 (nesting=1)
                doSomething();
            }
        }
    }                               // Cognitive Complexity = 3

**Special case - Preprocessor directives**: C/C++ preprocessor conditionals
(``#if``, ``#ifdef``, ``#elif``) are treated as structural increments. Unlike
regular ``if`` statements, they don't have braces, so nesting increases
immediately when encountered.

Loops
-----

**Keywords**: ``for``, ``foreach``, ``while``, ``do``

.. code-block:: java

    void processItems() {
        for (Item item : items) {       // +1 (nesting=0)
            while (item.hasNext()) {    // +2 (nesting=1)
                process(item.next());
            }
        }
    }                                   // Cognitive Complexity = 3

**Special case - do-while**: Only the ``do`` keyword increments; the trailing
``while`` is ignored to avoid double-counting.

Ternary Operator
----------------

**Operator**: ``? :``

The ternary operator acts like an ``if`` statement and receives a structural
increment. However, null-coalescing operators (``?.`` in C#, ``??`` in multiple
languages) are ignored because they improve readability.

.. code-block:: javascript

    // Ternary operator (counted)
    result = condition ? value1 : value2;   // +1

    // Null-coalescing (ignored)
    result = obj?.property;                 // +0
    result = value ?? defaultValue;         // +0

Hybrid Increments
=================

These structures receive a fundamental ``+1`` increment without nesting penalty,
but they DO increase the nesting level for code inside them.

Else Clauses
------------

**Keywords**: ``else``, ``elif``, ``elseif``

.. code-block:: python

    def check_value(x):
        if x > 100:             # +1
            return "high"
        elif x > 50:            # +1 (hybrid: no nesting penalty)
            return "medium"
        else:                   # +1 (hybrid: no nesting penalty)
            if x > 0:           # +2 (nesting=1 from else)
                return "low"
            else:               # +1 (hybrid)
                return "zero"
                                # Cognitive Complexity = 6

**Rationale**: When you read an ``else``, you've already paid the mental cost of
understanding the preceding ``if`` condition. The ``else`` represents an alternate
path, but doesn't require understanding a new condition (except for ``elif``).

However, code nested *inside* the ``else`` block does incur nesting penalties,
because it's contingent on the ``if`` condition being false.

Exception Handling
==================

Catch Clauses
-------------

**Keywords**: ``catch``, ``except``

Each ``catch`` clause receives a structural increment, regardless of how many
exception types it catches.

.. code-block:: java

    void handleRequest() {
        try {
            processRequest();
        } catch (IOException | SQLException e) {    // +1 (single increment)
            log(e);
        } catch (RuntimeException e) {              // +1
            handleError(e);
        }
    }                                               // Cognitive Complexity = 2

Try Blocks (Excluded from Nesting)
-----------------------------------

**Keywords**: ``try``, ``synchronized``

Try blocks and Java's ``synchronized`` blocks do NOT increment Cognitive
Complexity and do NOT increase nesting level for code inside them. This is a
critical exception to the nesting rules.

.. code-block:: java

    void myMethod() {
        try {
            if (condition1) {           // +1 (nesting=0, try doesn't count)
                for (int i = 0; i < 10; i++) {      // +2 (nesting=1)
                    while (condition2) { }          // +3 (nesting=2)
                }
            }
        } catch (Exception e) {         // +1 (nesting=0)
            if (condition3) { }         // +2 (nesting=1 from catch)
        }
    }                                   // Cognitive Complexity = 9

**Implementation note**: Track try blocks separately using ``cogc_excluded_nesting``
counter. Increment when entering a try block's opening brace, decrement when
exiting.

Switch Statements
=================

A ``switch`` statement and all its cases combined receive a single structural
increment.

.. code-block:: java

    String getWord(int number) {
        switch (number) {       // +1 (entire switch)
            case 1:
                return "one";
            case 2:
                return "two";
            case 3:
                return "three";
            default:
                return "many";
        }
    }                           // Cognitive Complexity = 1

**Rationale**: A ``switch`` compares a single variable against literal values,
which can often be understood at a glance. An ``if-else if`` chain can compare
different variables with different operators, requiring careful reading.

Fundamental Increments
======================

These structures receive ``+1`` regardless of nesting level and do NOT increase
nesting for their contents.

Binary Logical Operator Sequences
----------------------------------

**Operators**: ``&&``, ``||``, ``and``, ``or``

Cognitive Complexity increments for each *sequence* of like operators, not for
each individual operator. A sequence ends when the operator type changes.

.. code-block:: java

    if (a                   // +1 for if
        && b && c           // +1 (sequence of &&)
        || d || e           // +1 (new sequence of ||)
        && f)               // +1 (new sequence of &&)

    if (a                   // +1 for if
        &&                  // +1 (sequence of &&)
        !(b && c))          // +1 (new sequence of && inside negation)
                            // Total for both: +7

**Normalization**: ``&&`` and ``and`` are treated as equivalent, as are ``||``
and ``or``. This supports languages with both symbolic and keyword operators.

**Scope**: Binary logical sequences are counted everywhere, including variable
assignments and return statements:

.. code-block:: java

    boolean result = a && b && c;   // +1
    return x || y || z;             // +1

Jumps to Labels
---------------

**Keywords**: ``goto``, labeled ``break``/``continue``

- ``goto`` always increments by 1
- ``break`` or ``continue`` with a label increments by 1
- Unlabeled ``break``/``continue`` do not increment

.. code-block:: java

    OUT: for (int i = 0; i < max; i++) {    // +1
        for (int j = 0; j < i; j++) {       // +2 (nesting=1)
            if (i % j == 0) {               // +3 (nesting=2)
                continue OUT;               // +1 (labeled jump)
            }
        }
    }                                       // Cognitive Complexity = 7

Recursion
---------

Each method in a recursion cycle (direct or indirect) receives a fundamental
increment. This is not yet implemented in Lizard but is part of the specification.

Implementation in Lizard
========================

Pipeline Integration
--------------------

Cognitive Complexity is implemented as a Lizard extension in
``lizard_ext/lizardcogc.py``. The extension integrates into the token
processing pipeline as a generator that processes tokens sequentially:

::

    preprocessing → comment_counter → line_counter → token_counter →
    condition_counter → cognitive_complexity_counter (extension)

The extension processes tokens sequentially, maintaining state about nesting
levels, current operators, and pending structural elements.

Dual Nesting Tracking
----------------------

Lizard uses two parallel approaches to track nesting, taking the maximum of both:

1. **Bracket-based tracking** (``current_nesting_level``)

   - Uses ``NestingStack`` to track ``{`` and ``}`` braces
   - Works well for C-like languages
   - Adjusted by ``initial_nesting_level`` to exclude class/namespace containers

2. **Control-flow tracking** (``cogc_nesting_level``)

   - Explicitly incremented/decremented by structural keywords
   - Works for all languages, including brace-less (Python)
   - Required for languages where braces don't indicate cognitive nesting

**Nesting calculation** (from ``lizard_ext/lizardcogc.py``):

.. code-block:: python

    def add_cognitive_complexity(self, inc=1):
        # Calculate nesting from both approaches
        nesting_from_stack = max(0,
            self.current_nesting_level -
            self.current_function.initial_nesting_level - 1)
        nesting_from_cogc = self.cogc_nesting_level +
            self.current_function.initial_cogc_nesting_level

        # Take maximum, subtract excluded nesting (try blocks)
        nesting = max(nesting_from_stack, nesting_from_cogc) - \
                  self.cogc_excluded_nesting
        nesting = max(0, nesting)

        # Add increment plus nesting penalty
        self.current_function.cognitive_complexity += inc + nesting

Context State Variables
------------------------

The ``FileInfoBuilder`` context maintains these CogC-specific variables:

- ``cogc_nesting_level``: Current nesting depth for structural control flow
- ``cogc_last_operator``: Last binary logical operator seen (normalized)
- ``cogc_nesting_stack``: Boolean stack marking which nesting levels are structural
- ``cogc_excluded_nesting``: Count of try block nesting levels to exclude
- ``pending_lambda_nesting``: Flag that next brace should increase nesting for lambda
- ``lambda_depth``: How many lambdas deep (for JavaScript nested lambdas)

Function Boundary Handling
---------------------------

When entering a new function, Cognitive Complexity state is reset:

.. code-block:: python

    def reset_cogc_nesting(self):
        self.cogc_nesting_level = 0
        self.cogc_last_operator = None
        self.cogc_nesting_stack = []

The function also stores its initial nesting level to account for being nested
inside classes or namespaces:

.. code-block:: python

    self.current_function.initial_nesting_level = self.current_nesting_level

Language-Specific Adaptations
==============================

While the core rules are language-agnostic, certain language features require
special handling.

Nested Functions and Lambdas
-----------------------------

There are three approaches to handling nested functions, closures, and lambdas:

1. **Treat as separate** (Python nested functions)

   - Each function's complexity is calculated independently
   - Default for explicitly named nested functions

2. **Aggregate to parent** (JavaScript callbacks, C++ lambdas)

   - Complexity of the nested function adds to the parent
   - No separate ``FunctionInfo`` created for the nested function
   - Nested function increases nesting level for its contents

3. **Language-specific exceptions** (Python decorators, JavaScript outer functions)

   - Special patterns recognized and handled per Appendix A of specification
   - See sections below

Python Decorator Pattern
-------------------------

Python's decorator pattern uses nested functions to add behavior to existing
functions. To avoid penalizing idiomatic Python code, a narrow exception is made.

**Exception criteria**: A function is treated as a decorator wrapper (nesting=0
for the inner function) if it contains:

- Exactly one nested function definition
- Exactly one return statement
- No other statements except variable declarations

**Example**:

.. code-block:: python

    def a_decorator(a, b):
        def inner(func):        # nesting = 0 (decorator pattern)
            if condition:       # +1
                print(b)
            func()
        return inner            # total = 1

    def not_a_decorator(a, b):
        my_var = a * b          # Additional statement breaks pattern
        def inner(func):        # nesting = 1 (not a decorator)
            if condition:       # +1 structure, +1 nesting = +2
                print(b)
            func()
        return inner            # total = 2

**Implementation**: Track ``has_non_decorator_stmt`` flag in Python reader.
If true, nested functions receive nesting penalty.

JavaScript/TypeScript Outer Functions
--------------------------------------

Before ES6 classes became widespread, JavaScript used outer functions as
namespace or class-like containers. To avoid penalizing this pattern, outer
functions are treated specially.

**Declarative usage** (nesting=0 for nested functions):

.. code-block:: javascript

    function(...) {                 // Purely declarative; ignored
        var foo;

        bar.myFun = function(...) { // nesting = 0
            if (condition) {        // +1
                ...
            }
        }
    }                               // total complexity = 1

**Non-declarative usage** (nesting=1 for nested functions):

.. code-block:: javascript

    function(...) {                 // Non-declarative; counted
        var foo;
        if (condition) {            // +1; top-level structural increment
            ...
        }

        bar.myFun = function(...) { // nesting = 1
            if (condition) {        // +2 (nesting=1)
                ...
            }
        }
    }                               // total complexity = 3

**Detection**: If the outer function contains any top-level structural increments
(``if``, ``for``, etc.), it's non-declarative and nested functions get nesting
penalty.

**Implementation**: Track ``has_top_level_increment`` flag in TypeScript reader.
Set when structural keywords found outside nested functions. If true, use
``enter_lambda()`` to increase nesting for nested functions.

C++ Lambda Expressions
-----------------------

C++11 lambda expressions aggregate their complexity to the parent function. They
increase nesting level for code inside them but don't create a new ``FunctionInfo``.

.. code-block:: cpp

    void process() {
        auto lambda = [](int x) {       // +0, but nesting level now 1
            if (x > 0) {                // +2 (nesting=1)
                return x * 2;
            }
            return 0;
        };
    }                                   // Cognitive Complexity = 2

**Implementation**: Detect lambda via ``[`` capture list. Use ``enter_lambda()``
to set ``pending_lambda_nesting`` flag. On next ``{``, increase nesting without
structural increment.

Brace-less Languages
--------------------

Languages like Python and Ruby use indentation instead of braces. For these
languages, the ``cogc_nesting_level`` approach is essential.

**Key mechanism**: When a structural keyword is encountered, set
``pending_structural_nesting`` flag. When the language reader calls
``add_bare_nesting()`` for the indented block, check this flag and call
``increase_cogc_nesting()`` if set.

.. code-block:: python

    def example():
        if condition1:          # +1, pending_structural_nesting=True
                                # add_bare_nesting() → increase_cogc_nesting()
            for item in items:  # +2 (nesting=1)
                                # add_bare_nesting() → increase_cogc_nesting()
                if condition2:  # +3 (nesting=2)
                    process()

Preprocessor-Aware Languages
-----------------------------

C and C++ preprocessor directives (``#if``, ``#ifdef``, ``#elif``) are counted
as structural increments. However, they don't have braces, so nesting must
increase immediately.

.. code-block:: cpp

    #if DEBUG                   // +1, immediately increase nesting
    void debugFunction() {      // +0 (nesting from #if not counted for function)
        if (condition) {        // +3 (nesting=2: one from #if, one from function)
            doDebug();
        }
    }
    #endif                      // Cognitive Complexity = 4

**Implementation**: When preprocessor directive token is encountered, call both
``add_cognitive_complexity()`` and ``increase_cogc_nesting()`` immediately.

Implementation Checklist for New Languages
===========================================

When adding Cognitive Complexity support to a new language reader, ensure:

1. **Token Recognition**

   - Identify all structural keywords (``if``, ``for``, ``while``, etc.)
   - Identify hybrid keywords (``else``, ``elif``)
   - Identify binary logical operators (both symbolic and keyword forms)
   - Identify try/catch keywords
   - Identify ternary operator (if present)
   - Identify jump keywords (``goto``, labeled ``break``/``continue``)

2. **State Machine Integration**

   - Call ``add_cognitive_complexity()`` for structural increments
   - Set ``pending_nesting_increase`` flag for structures that increase nesting
   - Track special cases (``after_else``, ``in_do_while``, ``in_switch``)
   - Handle brace tracking or indentation-based nesting

3. **Nesting Management**

   - Ensure ``increase_cogc_nesting()`` called when entering structural blocks
   - Ensure ``decrease_cogc_nesting()`` called when exiting
   - Exclude try blocks from nesting (``cogc_excluded_nesting``)
   - Handle lambda/nested function nesting appropriately

4. **Function Boundaries**

   - Call ``reset_cogc_nesting()`` when starting a new function
   - Set ``initial_nesting_level`` to account for class/namespace containers
   - Decide how to handle nested functions (separate, aggregate, or exception)

5. **Testing**

   - Test each increment type (structural, hybrid, fundamental)
   - Test nesting at multiple levels (0, 1, 2, 3+)
   - Test edge cases (else-if, do-while, labeled jumps)
   - Test try-catch nesting exclusion
   - Test binary operator sequences
   - Validate against specification examples if available

Required Context Variables
---------------------------

Ensure your language reader has access to these context methods:

- ``add_cognitive_complexity(inc=1)`` - Add increment with nesting
- ``increase_cogc_nesting()`` - Increase nesting level
- ``decrease_cogc_nesting()`` - Decrease nesting level
- ``reset_cogc_nesting()`` - Reset at function boundary
- ``enter_lambda()`` - Mark lambda entry (if applicable)
- ``exit_lambda()`` - Mark lambda exit (if applicable)

And these context properties:

- ``cogc_nesting_level`` - Current nesting depth
- ``current_function.cognitive_complexity`` - Accumulator

Edge Cases and Common Pitfalls
===============================

Else-If Chain Detection
------------------------

**Problem**: Distinguishing ``else if`` from ``else { if }``.

**Solution**: Use ``after_else`` flag. Set true when ``else`` seen, check when
``if`` encountered. If true, don't add structural increment for the ``if``
(already counted as hybrid).

Do-While Loop Handling
-----------------------

**Problem**: Avoid double-counting ``do`` and trailing ``while``.

**Solution**: Use ``in_do_while`` flag. Set true when ``do`` seen, check when
``while`` encountered. If true, don't increment for ``while``.

Labeled Jump Detection
-----------------------

**Problem**: Distinguishing ``break`` from ``break LABEL``.

**Solution**: Use ``after_break_continue`` flag. Set true when ``break`` or
``continue`` seen. On next token, if it's not ``;`` or ``}`` or newline, count
it as labeled jump.

Null-Coalescing Operator Detection
-----------------------------------

**Problem**: Distinguishing ternary ``?`` from null-coalescing ``?.``.

**Solution**: Use ``saw_question_mark`` flag. Set true when ``?`` seen. On next
token, if it's ``.``, this was ``?.`` (ignore). Otherwise, it was ternary (count).

Binary Operator Sequence Tracking
----------------------------------

**Problem**: Counting sequences, not individual operators.

**Solution**: Track ``cogc_last_operator`` (normalized: ``&&``/``and`` → "and",
``||``/``or`` → "or"). Only increment when operator changes or is first in
statement.

**Reset points**: Reset ``cogc_last_operator`` to ``None`` at statement boundaries
(``;``, ``\n``, ``{``).

Initial Nesting Level Off-by-One
---------------------------------

**Problem**: Functions inside classes getting wrong nesting.

**Solution**: Store ``initial_nesting_level`` when function starts:

.. code-block:: python

    self.current_function.initial_nesting_level = self.current_nesting_level

Then subtract when calculating nesting:

.. code-block:: python

    nesting_from_stack = max(0,
        self.current_nesting_level -
        self.current_function.initial_nesting_level - 1)

The ``-1`` accounts for the function's own nesting level.

Try Block Nesting Exclusion
----------------------------

**Problem**: Try blocks should not contribute to nesting.

**Solution**: Maintain parallel ``try_block_stack`` alongside ``nesting_stack``.
When ``{`` follows ``try``, increment ``cogc_excluded_nesting``. When that ``}``
closes, decrement ``cogc_excluded_nesting``. Subtract from final nesting
calculation.

Lambda/Closure Nesting
----------------------

**Problem**: Lambda complexity should aggregate to parent, but lambdas should
increase nesting for their contents.

**Solution**: Use ``pending_lambda_nesting`` flag. When lambda detected, set flag.
On next ``{``, increase nesting WITHOUT structural increment. Track ``lambda_depth``
for nested lambdas.

Debugging Cognitive Complexity Calculations
============================================

When CogC values don't match expected results:

1. **Add calculation comments to test code**

   Show expected increments inline:

   .. code-block:: java

       if (condition1) {           // +1
           for (int i = 0; i < 10; i++) {  // +2 (nesting=1)
               while (condition2) { }      // +3 (nesting=2)
           }
       }                           // Expected: 6

2. **Enable debug output in cognitive_complexity_counter**

   Add print statements showing:

   - Token being processed
   - Current ``cogc_nesting_level``
   - Current ``current_nesting_level``
   - Increment applied
   - Running total

3. **Verify nesting stack consistency**

   Ensure ``cogc_nesting_stack`` has correct True/False markers for each
   nesting level. Length should match ``cogc_nesting_level``.

4. **Check initial_nesting_level**

   For functions inside classes, verify ``initial_nesting_level`` is set
   correctly to exclude the class container.

5. **Trace try block exclusion**

   Verify ``cogc_excluded_nesting`` increments for try blocks and decrements
   when they close.

6. **Compare against specification examples**

   Use tests from ``test/testCogCFromSpec.py`` which are direct translations
   of the specification examples.

Specification Compliance
=========================

This implementation aims for 100% compliance with the Cognitive Complexity
specification version 1.2 by SonarSource. Key specification sections:

- **1**: Three basic rules and four increment types
- **2**: Ignore shorthand (null-coalescing)
- **3**: Structural and hybrid increments (if/else)
- **4**: Catch clauses
- **5**: Switch statements
- **6**: Binary logical operator sequences
- **7**: Recursion (not yet implemented)
- **8**: Jumps to labels
- **9**: Nesting increments and try block exclusion
- **10**: Lambdas and nested methods
- **Appendix A**: Compensating usages (COBOL, JavaScript, Python)
- **Appendix B**: Formal specification enumeration
- **Appendix C**: Real-world examples with expected scores

Test Cases
----------

The file ``test/test_extensions/testCogCFromSpec.py`` contains test cases directly from the
specification. These tests are marked with:

::

    # SPEC REQUIREMENT: [description]


These are canonical examples and their expected values must not be changed to
match an incomplete implementation. If these tests fail, the implementation
needs to be fixed.

Conclusion
==========

Cognitive Complexity provides a more intuitive measure of code maintainability
than Cyclomatic Complexity by accounting for human judgment about mental effort.
The key insights are:

1. **Not all control structures are equal** - Switch is simpler than if-else chain
2. **Nesting matters** - Deeply nested code is harder to understand
3. **Shorthand helps** - Don't penalize readability aids
4. **Context matters** - else-if is simpler than the if was already understood

Lizard's implementation uses a dual-tracking approach to support
both bracket-based and control-flow-based nesting across diverse languages, with
special handling for language-specific patterns as outlined in the specification's
appendices.

When implementing Cognitive Complexity for a new language, follow the checklist
in this document, study the specification examples, and write comprehensive tests
before, during, and after implementation.

References
==========

- Cognitive Complexity specification v1.2 (2017) by SonarSource
- ``lizard_ext/lizardcogc.py`` for core implementation (extension)
- ``lizard_languages/`` for language-specific adaptations
