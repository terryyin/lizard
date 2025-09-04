### Goal
Add R language support to lizard to extract functions, parameters, NLOC, CCN, and nesting.

### Scope of R Syntax to Support (Scenarios)

- Functions
  - Named function by assignment: `name <- function(arg1, arg2) { ... }`
  - Named with `=`: `name = function(x) { ... }`
  - Anonymous function literals used as callbacks: `lapply(xs, function(x) x + 1)`
  - Nested functions: function defined inside another function body
  - Namespaced/dotted names: `pkg.name <- function() {}`, `S3.method.class <- function() {}`
  - Replacement and special names as plain identifiers: e.g., `name<- <- function(x, value) { ... }`

- Control flow and complexity (+1 each occurrence unless noted)
  - `if`, `else if` (aka `else if (cond)`), `else` (no increment)
  - `for`, `while`, `repeat`
  - Logical operators `&&`, `||` inside conditions
  - `switch` (each case adds complexity, if feasible in v1 treat presence as +1)

- Comments and tokens
  - Line comments: `# comment`
  - Strings: single `'...'`, double `"..."`, triple not used; raw strings not in base R; handle escapes
  - Multi-line strings via quotes spanning lines

### Non-goals (v1)
- Parsing formulas, quasiquotation, or NSE intricacies
- roxygen doc comment semantics (treated as comments)

### Test Snippets (Acceptance)

- Tokenization
  - `a/b, 123 /b` is not regex; tokens split as expected
  - Comments: `a # comment\n` → `['a', ' ', '# comment', '\n']`

- Functions
  - Named assign:
    `f <- function(x, y) { x + y }` → name `f`, params 2
  - Named with equals:
    `g = function() {}` → name `g`
  - Anonymous:
    `lapply(xs, function(x) x)` → one `(anonymous)` function
  - Assignment of anonymous:
    `h <- function(x) {}` → name `h`
  - Names with dot:
    `S3.method.class <- function(x) {}` → name `S3.method.class`
  - Nested:
    `outer <- function(a){ inner <- function(b){b}; inner }` → two functions `inner`, `outer`

- Complexity
  - `if`/`else if`:
    `f<-function(x){ if (x) {} else if (x&&y) {} else {} }` → CCN 3
  - Loops:
    `f<-function(){ for(i in 1:10){} while(TRUE){} repeat { break } }` → CCN 4

- NLOC and token count basic sanity via existing lizard metrics

### Implementation Notes
- Tokenizer: reuse ScriptLanguageMixIn.generate_common_tokens with R-specific additions if needed
- State machine:
  - Track `probable_name` when reading identifier followed by `<-` or `=` before `function`
  - On `function` keyword:
    - If just saw assignment to `probable_name`, use it; else use `(anonymous)`
    - Read parameters inside `(` `)`; push and then read body `{}` to end
- Register reader under extensions `['r', 'R']` and names `['r', 'rlang']`