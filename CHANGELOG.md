# Change Log

## Unreleased

### Bug Fixes
- JavaScript, TypeScript, JSX, and TSX: count each `??` and `??=` as +1 CCN; existing scores may decrease (issue #507)
- JavaScript, TypeScript, JSX, and TSX: count each default parameter and destructuring initializer as +1 CCN, including nested defaults; existing scores and threshold warnings may increase (issue #509)
- Swift: stop `.init(`, `self.init(`, `.get()`, `.set(`, variables named `set`, and argument labels such as `init:` from starting phantom functions that swallowed the following functions
- Swift: recognize argument labels such as `init:` and `for:` when a newline or comment separates them from `(` or `,`
- Swift: tokenize `#Preview`, `#expect`, `#available` and other macros so their braces count; compiler directives stay whole lines
- Swift: scan raw, multi-line, and interpolated string literals and nested block comments as single tokens, so braces inside them no longer shift function boundaries
- Swift: do not treat `type` (`.type`, `type(of:)`) as a Go-style type declaration

## 1.24.1

### New Features
- **Cognitive Complexity** (`-Ecognitive`) — per-function Cognitive Complexity following SonarSource's specification (structural, hybrid, fundamental and nesting increments; `switch` and logical operator sequences count once; lambdas nest; direct recursion counts), with a `CogC` column, a `--CogC` threshold and a `cognitive_complexity` field for `-s`/`-T`. Implemented as an extension that leaves the language readers and all existing metrics untouched (issue #432)

### Bug Fixes
- Python: preserve function names and signatures for PEP 695 generic functions, including nested type parameter bounds (PR #492)
- Kotlin: correctly finish expression-bodied functions and accessors, including bodies containing `when` expressions (issue #493)
- Rust: count `match` arms toward cyclomatic complexity and handle nested match expressions (issue #494)
- PHP: end trait methods at their closing brace instead of extending them to the end of the trait (issue #498)
- TypeScript and TSX: preserve function boundaries after regex arguments and template interpolations containing quoted backticks (issue #497)
- JavaScript, TypeScript, and TSX: tokenize regex literals containing quotes, escapes, and character classes without swallowing following code (PR #505)

### Documentation
- Document Cognitive Complexity usage, thresholds, and language coverage
- Update contributor guidance and adopt the Open Dough agent workflow skills

## 1.24.0

### New Features
- **Halstead metrics** (`-Ehalstead`) — per-function Halstead volume, difficulty, and effort (issue #464, PR #485)
- **`--no-gitignore`** — analyze all discovered source files even when a `.gitignore` would exclude them (PR #488)
- **PHP** — modern syntax is parsed without false functions (classes, traits, visibility, constructor property promotion, match expressions, arrow functions, union types, named arguments); null-coalescing / nullsafe operators no longer inflate nesting depth (issue #491)

### Bug Fixes
- Java: do not report control structures in a static block as methods (issue #312, PR #489)
- Java: count anonymous classes in field initializers (issue #311, PR #483)
- Java: treat `record` as a contextual keyword in field initializers and method declarations
- Java: parse generic and qualified type names in anonymous classes
- Go: register generic functions with `[...]` type parameters (PR #484)
- CSV: emit columns for extensions that add multiple `FUNCTION_INFO` fields (PR #486)
- Python: count control flow inside f-string interpolations (issue #317, PR #481)
- Script: stop a `#` comment continuing past a trailing backslash (issue #317, PR #482)
- Objective-C: handle nested parentheses in block / function-pointer parameter types (issue #365, PR #480)

## 1.23.0

### New Features
- **Python** — structural pattern matching (`match`/`case`, PEP 634, Python 3.10+) is now counted correctly:
  - Each `case` arm adds +1 to cyclomatic complexity (like an `if`/`elif` branch)
  - `case` guards (`case x if cond:`) count the `if` as a normal condition
  - `case` and `match` used as plain variable names (assignments, attribute access, function calls, subscripts, tuple unpacking, annotated assignments) are not counted — soft-keyword disambiguation via lookahead in `preprocess`
  - `--modified`: an entire `match`/`case` block counts as 1, consistent with `switch`/`case` in C-family languages

### Bug Fixes
- C/C++: stop raw string literals (`R"(…)"`) from swallowing following code in the tokenizer (issue #478, PR #479)

## 1.22.2

### Bug Fixes
- TypeScript: handle function stack correctly when starting a new function (avoids incorrect nesting / metrics)
- Duplicate finder: avoid double-counting overlapping duplicate code ranges (PR #474)

### Security
- Demo Flask app (`index.py`): enable debug only when `FLASK_DEBUG` is set, not by default (PR #475)

## 1.22.1

### Bug Fixes
- TypeScript: prevent `IndexError` when parsing nested template literals (issue #471, PR #472)

## 1.22.0

### Improvements
- **TypeScript, TSX, and JSX** — parsing and metrics are much closer to real code (PRs #467, #468):
  - **TSX/JSX** use the same `TypeScriptStates` path as `.ts` (tokenizer-only layer for JSX), so **class methods** and **CCN** are no longer wrong or double-counted.
  - **Skips** that reduce false functions: `interface { … }` method signatures, `type … =` value types, `abstract` method declarations without bodies; **ES2022** private names `#foo` in tokenization; **smarter parameters** (type keyword noise, commas inside `Map<…>`, etc.).
  - **Class field arrows** (`handleClick = () => {}`) are reported under the **field name** instead of `(anonymous)`; better **call vs definition** and class-body cases (typed fields, `static` / `async` fields, LWC-style `async x =>` fields, `field = CONST.PROP;`, JSX attribute expressions).

## 1.21.7

### Bug Fixes
- Java: treat `record` as a contextual keyword (field and method name `record` are no longer parsed as a record class); track brace depth for field array/object initializers so `= { }` does not end the class body before a `static` block (issue #470)

## 1.21.6

### Improvements
- Release workflow: clarify how PyPI matches trusted publishers (repository owner id, workflow file, environment); add optional `PYPI_API_TOKEN` secret for token-based upload when OIDC is not used

## 1.21.5

### Improvements
- Release workflow: publish to PyPI without a GitHub Environment so trusted publishing matches PyPI’s default GitHub publisher settings (owner, repository, `release.yml`, empty environment name)

## 1.21.4

### Bug Fixes
- Fix Java parsing when a field initializer uses a class literal (`Type.class`), which could mis-parse the next method (e.g. `catch` treated as a method name) (issue #469)
- Fix Java static initializer blocks (`static { ... }`) so control-flow keywords inside the block are not counted as methods (issue #469)
- Fix Java double-brace anonymous classes (`new Foo() {{ ... }}`) so instance-initializer bodies are not parsed as class methods (issue #469)

## 1.21.3

### Bug Fixes
- Fix Java annotations with parenthesized arguments (e.g. `@Transactional(rollbackFor = Exception.class)`) being parsed as methods and corrupting complexity (issue #463)

## 1.21.2

### Bug Fixes
- Fix nesting depth calculation for C++ `else if` chains (issue #418)
  - `else if () {}` is now treated as same nesting level as `if`, not as nested
  - Matches behavior of similar tools and user expectations

## 1.21.1

### Bug Fixes
- Fix Ruby parser hang on %i[] and %I[] symbol array literals (issue #457)
- Fix regex in CodeReader to prevent catastrophic backtracking on multiple question marks after less than sign (issue #459)

### Improvements
- Add script directory to sys.path for running lizard.py from source (issue #460)

## 1.21.0

### New Features
- Add selective metric forgiveness (issue #455)
  - Use `#lizard forgives(length)` to forgive only specific metrics
  - Use `#lizard forgives(length, parameter_count)` for multiple metrics
  - `#lizard forgives` without parentheses continues to forgive all metrics (backward compatible)

### Bug Fixes
- Fix PHP parser incorrectly treating "use function" imports as function declarations (issue #442)
  - PHP parser now correctly ignores function names in "use function" statements
  - Function names are no longer overridden by imported function names
- Fix Java parser incorrectly treating "record" variable names as keywords (issue #453)
  - Java parser now correctly distinguishes between the `record` keyword and variables named "record"
  - Variables named "record" inside method bodies are no longer misinterpreted as class declarations
- Fix C++ lambda parsing state machine issues (issue #443)
  - Fixed lambda capture state incorrectly transitioning to global state instead of parameter parsing
  - Added proper bracket tracking for lambda parameter lists and bodies
  - Improved handling of nested brackets within lambda expressions
  - Added support for lambda qualifiers (mutable, noexcept, constexpr, consteval)
  - Added test case for multiple functions with static_cast expressions

## 1.20.0

### Bug Fixes
- Fix IndexError crash when parsing C++ raw string literals containing braces (issue #451)
  - Added proper tokenization for C++ raw string literals (R"delimiter(content)delimiter")
  - Enhanced lizardns extension with defensive invariant protection
  - Prevents misinterpretation of braces within string literals as structural elements

### Improvements
- Improved robustness of nested structures counting extension
- Better error handling for edge cases in tokenization

## 1.19.0

### New Features
- Add PL/SQL language support with comprehensive parsing for procedures, functions, and triggers
- Add support for schema-qualified names in PL/SQL parser

### Improvements
- Enhanced JavaScript and TypeScript parsing with improved static and async method detection
- Better method call detection to avoid false positives in JavaScript/TypeScript
- Improved previous token context tracking for accurate function detection
- Reorganized supported languages list in README alphabetically

### Bug Fixes
- Fix line count calculation issue when encountering empty lines after macro definition continuation markers

### Documentation
- Add contribution guidelines to README

## Earlier releases

See [the changelog archive](docs/changelog-archive.md) for versions 1.18.0 and earlier.
