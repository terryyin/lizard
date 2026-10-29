# Dart language support

**Mapped story identity:** SEED-021#dart-support
**Source:** [canonical story](../../seeds/SEED-021-dart-support.md#dart-support)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

Dart and Flutter developers analyze `.dart` sources through the existing CLI
and Python API and get each function and method, under a class-qualified name,
with CCN, NLOC, token count, and parameter count, so they can find the methods
that need review. The story's Scope section is authoritative for required
behavior, deferred promises, and boundary assumptions.

**In scope:** a new `DartReader` in `lizard_languages/dart.py`, registered in
`lizard_languages/__init__.py`; Dart tokens (`name?`, `??`, `??=`, `?.`,
`?..`, raw and triple-quoted strings); Dart declaration states (expression
bodies, `get`/`set` accessors, constructors with initializer lists and
`factory`, `async*`/`sync*` modifiers); decision rules for exception clauses
and switch-expression arms; the README language list; tests under
`test/test_languages/testDart.py`.

**Excluded (story's deferred promises):** decisions inside `${…}`
interpolation; separate `(anonymous)` reporting of closures; verification of
extension metrics; verification of `if-case`, records, and extension types.

**Architecture / PFE:** The reader is Lizard's established extension point
(ADR-0001 *Language reader*: inherits `CodeReader`, registered in
`lizard_languages/__init__.py`), so no ADR or North Star change is needed.
Reuse found across the product:

- `CLikeReader` + `CLikeStates` recognise Dart's `Type name(params) {` shape
  and `CLikeNestingStackStates` already qualifies members as `Class::member`
  (observed, premise P1). Dart states extend `CLikeStates`, not `JavaStates`:
  the Java-specific record, annotation-argument and anonymous-class handling
  is not needed, and the Java reader leaks `class_name` onto later top-level
  functions (`Point::fetch` in the probe).
- Kotlin and TypeScript tokenizer additions keep `\w+\?`, `??`, and `?.` as
  single tokens; Dart adds `??=` and `?..`. Kotlin's `_expression_body`
  state is the model for `=>` bodies, simplified because Dart's body ends at
  `;` at bracket depth zero.
- Swift's `literal_spans` shows a span-based tokenizer for multi-line string
  literals; Dart needs only two extra regex alternatives for `'''…'''` and
  `"""…"""` plus `r` prefixes, so prefer regex additions and reach for spans
  only if the regex cannot stay subgroup-free.
- The `test/test_languages/kotlin_helpers.py` pattern (`get_<lang>_function_list`
  via `analyze_file.analyze_source_code`) is the test entry point; CLI
  discovery follows `test/testApplication.py` conventions.

Naming follows ADR-0001's alignment policy: capability names (`DartReader`,
`dart.py`, `testDart.py`), no phase numbers in product code.

**Assumptions:** Dart 3 syntax is the target; Lizard stays free of any Dart
toolchain; the unformatted ternary `a?b:c` reads as nullable, a limit shared
with the Kotlin and Swift readers and stated in the story.

## Outside-in proof

Primary boundary: `analyze_file.analyze_source_code("a.dart", source)` through
a `get_dart_function_list` helper in `test/test_languages/testDart.py`, one
test per key example, asserting names, CCN, NLOC, and parameter counts. CLI
discovery and `-l dart` selection are proved at the `lizard.main` boundary
with a temporary directory holding a `.dart` and a `.py` file (filesystem
boundary only; no parsing mocks).

Local gates, cited from `AGENTS.md`: focused
`nix develop -c python -m pytest test/test_languages/testDart.py` during each
slice; before completing the change, the full suite and format check
`nix develop -c make` (pytest with coverage, `pycodestyle`, `pylint`). Hosted
CI (`python-app.yml`) runs pytest across Python 3.8–3.14 after publication;
avoid syntax newer than 3.8.

| Promise (story scope) | Owning slice | Proof signal |
| --- | --- | --- |
| `.dart` discovered, `-l dart`, help lists `dart`, README | 1 | CLI test on temp dir; help text contains `dart`; README line present |
| Block-bodied top-level functions and methods, `Class::member`, parameters, closures inside enclosing function, interpolation inert (examples 2, 7-first, 9) | 1 | `greet` CCN 3 / 3 params / NLOC 7; `MyWidget::build` CCN 3, no `(anonymous)`; full suite green |
| Nullable and null-aware tokens add nothing; `??`/`??=` count once (example 3) | 2 | `maybe` CCN 2, NLOC 1, 2 params |
| Raw and triple-quoted strings and comments inert (example 8) | 3 | CCN 1; following function recognised; comment excluded from NLOC |
| `=>` bodies, getters, setters, names `sum` / `scale=` (examples 3, 4 accessors) | 4 | `Point::sum` CCN 1 NLOC 1; `Point::scale=` CCN 2; `Point::isOrigin` CCN 2; `Point::toString` |
| Constructors: body-less not reported, initializer list, `factory`, `Point::Point`, `Point::fromJson`, `Point::checked` (example 4) | 5 | listed functions and counts exactly; no function spanning two members |
| Exception clauses once each; switch-expression arms, `_` excluded, no arm functions (examples 5, 6) | 6 | `classify` CCN 4; `size` CCN 4 and one function; try/on/catch CCN 4 |
| `async*` / `sync*` keep the function (example 7-second) | 7 | `gen` and `after` both reported |

## Settled decisive premises

Observed on 2026-10-29 in this workspace
(`/Users/terryyin/git/lizard/.worktrees/support-dart-analysis`, branch
`claude/support-dart-analysis` at `30ccf81`), using a scratch runtime
`CLikeReader` subclass with `ext = ['dart']` appended to `languages()` in a
one-off Python process; no file in the checkout was edited.

| # | Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- | --- |
| P1 | A `CLikeReader`-derived reader registered for `.dart` is selected by `analyze_source_code` and `lizard.main`, recognises block-bodied functions and methods, qualifies members as `Class::member`, counts closure decisions in the enclosing function, and leaves `${…}` inert | 1, 7 | Sample file: `add` CCN 1 / 2 params; `greet` CCN 3 / 3 params / NLOC 7; `Point::scale` CCN 2; `classify` CCN 4; `closures` CCN 2 (the `if` inside a closure); `lizard -l dart <dir>` listed only `main.dart` functions while the default run also listed `tool.py`'s `f`; `language_names[0]` join contains `dart` | True |
| P2 | Bare `?` tokens inflate CCN under C-like counting | 2 | `int? f(int? x) { return x ?? 0; }` → CCN 3 (expected 2); tokens `['int','?',…,'?','?','0']`; `a ??= b` → `'?','?','='`; `a?..c()` → `'?','.','.'` | True |
| P3 | Triple-quoted strings leak their contents | 3 | `'''it's ? a : b'''` tokenised as `''`, `'it'`, `s ? a : b`, `''`, `'`; enclosing `f` CCN 2 instead of 1. `r'raw'` tokenises as `r` + string, harmless to CCN but counts an extra token | True |
| P4 | `=>` bodies and `get`/`set` accessors are not recognised | 4 | `maybe`, `sum`, `isOrigin`, `toString`, `twice` absent from the probe's function list | True |
| P5 | A Dart initializer list swallows the next member | 5 | `Point.origin() : x = 0, y = 0;` followed by `factory Point.fromJson(...) {…}` reported as one function `Point::origin` at lines 22–28 (the factory's lines) | True |
| P6 | `on T { }` without `catch` is not counted; switch-expression `=>` is one token | 6 | try with `on FormatException {}`, `on IOException catch (e) {}`, `catch (e) {}` → CCN 3 (expected 4); `switch (n) { 0 => 'z', _ => 'b' }` tokens include `'=>'` twice as single tokens | True |
| P7 | `async*` loses the function; `async` keeps it | 7 | `Stream<int> gen() async* {…}` then `int after() {…}` → only `after`; `Future<int> go() async {…}` → `go` | True |
| P8 | Nothing in product code or tests references Dart | 1 | `grep -rni dart` over `*.py`, `*.md`, `*.rst` outside planning and skills: no matches; no test enumerates `languages()` | True |

## Ordered slices

### 1. Dart files are discovered and analyzed with C-like function recognition
Type: Behavior
Status: done
Proof: `testDart.py`: `greet` → CCN 3, 3 parameters, NLOC 7; `add` → CCN 1;
`MyWidget::build` with an `onPressed: () { if … }` closure and a ternary → one
function, CCN 3, no `(anonymous)`; CLI test: temp dir with `lib/main.dart` and
`tool.py`, `lizard -l dart` reports only Dart functions, default run reports
both, `-h` output lists `dart`; README lists Dart; full suite green.

Behavior: Given a `.dart` file → analyze through API or CLI → its block-bodied
top-level functions and methods are reported under `Class::member` names with
CCN, NLOC, token and parameter counts, closures counted in the enclosing
function, `${…}` contents inert.

Interim behavior (named, replaced later): nullable `?` inflates CCN until
slice 2; `'''…'''` leaks until slice 3; `=>` bodies and accessors are missed
until slice 4; constructors with initializer lists misparse until slice 5;
`on T` clauses undercount and switch-expression arms are uncounted until slice
6; `async*`/`sync*` lose the function until slice 7.

Files: `lizard_languages/dart.py` (`DartReader(CLikeReader)` with
`ext = ['dart']`, `language_names = ['dart']`, `DartStates(CLikeStates)`
initially empty, parallel states `DartStates` + `DartNestingStackStates`
extending type keywords with `mixin`/`enum` via `_TYPE_KEYWORDS`),
`lizard_languages/__init__.py`, `README.rst`,
`test/test_languages/testDart.py` (+ `dart_helpers.py` if the Kotlin helper
pattern is followed).

### 2. Nullable and null-aware operators are not decisions; `??` and `??=` count once
Type: Behavior
Status: done
Proof: `int? maybe(int? x, [int fallback = 0]) => x ?? fallback;` is covered
after slice 4; for this slice use a block body: `int f(int? x) { return x ?? 0;
}` → CCN 2; `a?.b`, `a?..c()`, `x!` → CCN 1; `a ??= b` → CCN 2;
`List<int?> xs` in a body → CCN 1; `x = y ? z : w;` → CCN 2 still.

Behavior: Given Dart null-safety operators → analyze → `T?`, `?.`, `?..`,
and `!` add nothing; `??` and `??=` add one each; the spaced ternary still
adds one.

Design: tokenizer additions `\w+\?`, `\?\?=`, `\?\?`, `\?\.\.`, `\?\.`
(longest first, subgroup-free); `_logical_operators = {'&&', '||', '??',
'??='}`; `>?` closing generic handled by the existing `<…?>` generic token.

### 3. Raw and triple-quoted strings and doc comments are inert
Type: Behavior
Status: planned
Proof: function containing `var m = '''it's ? a : b''';`, `var r = r'$x ? y :
z';`, `var d = """ ? """;` and preceded by `/// if while` → CCN 1; NLOC
excludes the comment line; a following `int g() { return 2; }` is still
reported.

Behavior: Given Dart string and comment forms → analyze → their contents
produce neither decisions nor functions, and comments leave NLOC.

Design: tokenizer alternatives for `(?:r)?'''[\s\S]*?'''`,
`(?:r)?"""[\s\S]*?"""`, and `r'…'` / `r"…"` ahead of the base string rules;
`///` is already a `//` comment via `CCppCommentsMixin`.

### 4. Expression-bodied functions and `get`/`set` accessors are functions
Type: Behavior
Status: planned
Proof: `int? maybe(int? x, [int fallback = 0]) => x ?? fallback;` → `maybe`,
CCN 2, 2 params, NLOC 1; in `class Point`: `int get sum => x + y;` →
`Point::sum` CCN 1 NLOC 1; `set scale(int f) { if (f > 0) { print(f); } }`
→ `Point::scale=` CCN 2, 1 param; `bool get isOrigin { return x == 0 && y ==
0; }` → `Point::isOrigin` CCN 2; `@override String toString() => 'Point($x,
$y)';` → `Point::toString` CCN 1; `extension on int { int twice() => this *
2; }` → `twice` CCN 1; an arrow body containing `items.map((e) => e * 2)` →
still one function ending at the `;`.

Behavior: Given `=>` bodies and accessors → analyze → each is one function,
ending at `;` at bracket depth zero, named by property (`sum`, `scale=`).

Design: in `DartStates`, `_state_dec_to_imp` accepts `=>` → expression-body
state (Kotlin model, `;`-terminated, depth-tracked); `get` at declaration
position followed by an identifier starts a function named by the identifier
(block or arrow body); `set` likewise with `=` appended to the name and the
parameter list read as usual. `get`/`set` used as ordinary identifiers (e.g.
`map.get(...)`, `set.add(x)`) are followed by `.` or `(`, not an identifier,
and fall through unchanged.

### 5. Constructors are reported only with a body, and initializer lists end at the body or `;`
Type: Behavior
Status: planned
Proof: the story's `class Point` → exactly `Point::fromJson` (CCN 2, 1 param),
`Point::sum`, `Point::scale=`, `Point::isOrigin`, `Point::toString`; no
function spans lines of two members; `Point.checked(this.x, this.y) { if (x <
0) { throw ArgumentError(); } }` → `Point::checked` CCN 2, 2 params; `Point(this.x,
this.y) { assert(x >= 0); }` → `Point::Point`; `Point.origin() : x = 0, y =
0;` and `Point(this.x, this.y);` → not reported; `const Point.zero() : x = 0,
y = 0;` → not reported.

Behavior: Given Dart constructors → analyze → body-less declarations are not
functions; `: initializer, initializer` (including `super(...)`, `this(...)`,
`assert(...)`) is skipped to the `{`, `=>`, or `;` that follows; `factory` and
`const` modifiers are transparent; a named constructor `Point.fromJson` reports
as `Point::fromJson` and the unnamed one as `Point::Point`.

Design: `DartStates._state_initialization_list` replaces the C++ one: read at
depth zero until `{` (enter body), `=>` (expression body), or `;` (no body);
`Point.named(` in declaration position: the `.` after a name that matches the
current class name joins the constructor name, then the second segment is kept
as the member name. Class name comes from `CLikeNestingStackStates`'s
namespace tracking (already producing `Point::`).

### 6. Exception clauses count once each; switch-expression arms count without creating functions
Type: Behavior
Status: planned
Proof: `try { a(); } on FormatException { b(); } on IOException catch (e) {
c(); } catch (e) { d(); }` in a function → CCN 4; `String size(int n) =>
switch (n) { 0 => 'zero', 1 || 2 => 'small', _ => 'big' };` → one function
`size`, CCN 4; `String classify(int n) { switch (n) { case 0: … case 1: case
2: … default: … } }` → CCN 4; a switch expression used as a statement
initializer inside a block body → same counts, no extra functions.

Behavior: Given Dart exception clauses and switch expressions → analyze →
each clause adds one whether `on T`, `catch (e)`, or `on T catch (e)`; each
switch-expression arm adds one except a bare `_` arm; `=>` inside a switch
expression never starts or ends a function.

Design: `on` added to control-flow keywords with `catch` suppressed when it
follows an `on` clause's type (or: count `on`, and count `catch` only when the
previous significant token is `}` or `)` of a try block, not a type name); in
the expression-body and body states, `switch (` … `{` opens a switch
expression context where `=>` at that depth increments CCN unless the arm
pattern is exactly `_`. Statement `switch` keeps the `case` rule.

### 7. Generator and async modifiers keep the function
Type: Behavior
Status: planned
Proof: `Stream<int> gen() async* { yield 1; }` followed by `int after() => 1;`
→ both `gen` and `after`; `Iterable<int> syn() sync* { yield 1; }` → `syn`;
`Future<int> go() async { … }` and `Future<int> arrow() async => 1;` → both
reported; Flutter example `MyWidget::build` from slice 1 re-asserted with an
`async` callback `onPressed: () async { if (ok) { await run(); } }` → CCN 2
and one function.

Behavior: Given `async`, `async*`, or `sync*` between the parameter list and
the body → analyze → the function is recognised with the same name, metrics,
and body boundaries as without the modifier.

Design: `DartStates._state_dec_to_imp` consumes `async`, `sync`, and a
following `*` before `{` or `=>`.

Safe stop: after any slice the suite is green and earlier slices' behavior
holds; interim behavior named in slice 1 shrinks slice by slice.

## Current decisions

- Extend `CLikeStates`, not `JavaStates`; qualification via
  `CLikeNestingStackStates`.
- Setter name carries `=` (`Point::scale=`); getter uses the bare property
  name; constructors use `Class::Class` and `Class::named`.
- Closures stay inside the enclosing function (story decision; deferred
  promise records the alternative).
- `??` and `??=` are logical-operator decisions; `?.`, `?..`, `!`, `T?` are
  not.
- Switch-expression arms count except `_`; `default` and `else` never count.
- Tokenizer additions stay subgroup-free regex alternatives
  (`token_pattern.py` rule) unless triple-quoted strings force a span scan.

## Learnings

- Minimal `DartReader(CLikeReader)` with empty `DartStates` plus
  `DartNestingStackStates` is enough for block-body recognition; mixin/enum
  need nesting type-keywords, while `extension` stays unqualified for slice 4's
  bare `twice`.
- Nesting keywords are shared via overridable
  `CLikeNestingStackStates._TYPE_KEYWORDS` (post-change refactor).

## Concern evidence

- Slice-plan refinement: not needed — each slice is one Behavior with its own
  proof loop and a safe stop; no Structure preparation is required because
  the C-like base already supplies the shared recogniser; decisive premises
  P1–P8 are observed in this workspace.
- Remaining slice-specific concerns: none identified. Slice 6's two rules
  (exception clauses, switch-expression arms) share one proof loop around
  decision counting of Dart-specific branch syntax; split them during
  execution only if the switch-expression context handling overruns.
- Interim behavior named in slice 1 is an accepted trade-off with its
  replacing slices listed.

## Story obligations

### G1. Nullable and null-aware tokens inflate or mis-split CCN
Reported: slice 1 — "nullable `?` inflates CCN until slice 2"
Story clause: "Not decisions: nullable `T?`, `?.`, `?..`, `!`"
Disposition: proved by slice 2: test/test_languages/testDart.py TestDartNullSafetyOperators

### G2. Triple-quoted / raw strings and doc comments not yet inert for NLOC
Reported: slice 1 — "`'''…'''` leaks until slice 3"
Story clause: "Their contents introduce neither decisions nor functions; comment lines are excluded from NLOC"
Disposition: interim until slices 3
Commitment: 31.3, 31.4, 31.5, 31.8, 32

### G3. Expression bodies are missed
Reported: slice 1 — "`=>` bodies are missed until slice 4"
Story clause: "Block bodies and `=>` expression bodies both form a function"
Disposition: interim until slices 4
Commitment: 17

### G3b. Getters and setters are missed
Reported: slice 1 — "accessors are missed until slice 4"
Story clause: "getters (`get name`), setters (`set name(v)`)"
Disposition: interim until slices 4
Commitment: 13, 14, 26, 27

### G4. Constructors with initializer lists misparse; body-less not excluded yet
Reported: slice 1 — "constructors with initializer lists misparse until slice 5"
Story clause: "A declaration with no body, such as `Point(this.x, this.y);` or `Point.origin() : x = 0, y = 0;`, is not a function"
Disposition: interim until slices 5
Commitment: 21

### G5. Exception `on T` clauses undercount
Reported: slice 1 — "`on T` clauses undercount until slice 6"
Story clause: "each exception clause once whether written `on T`, `catch (e)`, or `on T catch (e)`"
Disposition: interim until slices 6
Commitment: 28.5

### G5b. Switch-expression arms uncounted
Reported: slice 1 — "switch-expression arms are uncounted until slice 6"
Story clause: "In a switch expression each arm counts once except a bare"
Disposition: interim until slices 6
Commitment: 29

### G6. `async*` / `sync*` lose the function
Reported: slice 1 — "`async*`/`sync*` lose the function until slice 7"
Story clause: "`async`, `async*`, and `sync*` between the parameter list and the body do not break recognition"
Disposition: interim until slices 7
Commitment: 19, 20

### G7. Interpolation decisions deferred
Reported: slice 1 — "Decisions inside string interpolation are deferred"
Story clause: "Decisions inside string interpolation `${…}` are not counted"
Disposition: excluded "Decisions inside string interpolation `${…}` are not counted"
Commitment: X1

### G8. Separate anonymous closure reporting deferred
Reported: slice 1 — "Separate anonymous closure reporting is deferred"
Story clause: "Reporting closures and function literals as separate `(anonymous)` functions, as the JavaScript and Kotlin readers do"
Disposition: excluded "Reporting closures and function literals as separate `(anonymous)` functions, as the JavaScript and Kotlin readers do"
Commitment: X2

### G9. Extension metrics beyond core four not verified
Reported: slice 1 — "Extension metrics beyond the core four are not verified"
Story clause: "Extension metrics beyond the core four (nesting depth, and so on) are whatever the shared machinery yields; not verified here"
Disposition: excluded "Extension metrics beyond the core four (nesting depth, and so on) are whatever the shared machinery yields; not verified here"
Commitment: X3

### G10. Unlisted Dart 3 forms without dedicated verification
Reported: slice 1 — "Unlisted Dart 3 forms follow the common rule without dedicated verification"
Story clause: "`if (x case pattern)`, records, extension types, and other Dart 3 forms not in the key examples follow the common rule without their own verification"
Disposition: excluded "`if (x case pattern)`, records, extension types, and other Dart 3 forms not in the key examples follow the common rule without their own verification"
Commitment: X4
