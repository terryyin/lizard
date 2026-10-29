---
id: SEED-021
status: active
planted: 2026-10-29
planted_during: Product backlog intake requested by the developer
scope: Dart language support
---

# SEED-021: Dart support

## Why This Matters

Dart and Flutter developers need to identify complex code in their codebases
using the same Lizard workflow they use for other languages. The request is
long-standing: GitHub issue #325 (2021) asked for Dart/Flutter support and was
closed without a reader. Dart is C-like in declaration shape, so most of the
existing C-like recognition already applies; what is missing is Dart's own
syntax for null safety, expression bodies, accessors, constructors, switch
expressions, and string forms.

## Alternatives and Decision

Compared against the goal and the constraint that Lizard readers are
self-contained token/state-machine parsers (ADR-0001, *Language reader*):

- **Recommended: a new `DartReader` built on `CLikeReader` and `CLikeStates`,
  adding Dart-specific tokens and declaration states.** A scratch C-like reader
  registered for `.dart` already recognised block-bodied top-level functions
  and methods with class-qualified names and correct CCN on the sample
  (observed in this workspace). The gaps are all Dart-specific and bounded:
  `?` tokens inflate CCN, `=>` bodies and `get`/`set` accessors are missed,
  Dart initializer lists swallow the following member, `async*`/`sync*` lose
  the function, and `'''…'''` strings leak their contents.
- Registering `.dart` on the existing Java reader with no new code: fails the
  same gaps and leaks the class name onto top-level functions. Rejected as
  insufficient.
- A reader on the `GoLikeStates` family (Kotlin, Swift): needs a function
  keyword, which Dart lacks; the Kotlin reader found only two `get` fragments
  in the sample. Rejected.
- Reusing the TypeScript reader for its `=>`/`??` handling: Dart declarations
  have no `function` keyword and the TS parser is organised around JavaScript
  binding forms; it found seven mostly wrong fragments. Rejected.
- Delegating to the Dart analyzer toolchain: contradicts the self-contained
  reader architecture every existing language follows. Rejected.

Borrowed within the product: the Kotlin/TypeScript tokenizer additions that
keep `name?`, `??`, and `?.` as single tokens, and Kotlin's expression-body
state (simpler for Dart, whose `;` terminates the body). Known shared limit:
an unformatted ternary written `a?b:c` reads as a nullable and is not counted,
the same limit the Kotlin and Swift readers accept; `dart format` always
spaces a ternary.

## Selected Stories

<a id="dart-support"></a>

### 1. Support Dart analysis

**Identity:** SEED-021#dart-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/014-dart-support/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"a7bb0decd29cf8f7092c9ebfba9fefcac56bbf8daedf398ed8cea2c02eae8aac","plan":"211ac0b670cb09cfef77b19cce03c8340f881a54f9387b23bfe787df4a471fbf"}}
```

**Goal:** Dart and Flutter developers, and maintainers of mixed-language
codebases, point Lizard at their Dart sources through the existing CLI and
Python API and receive each function and method with CCN, NLOC, token count,
and parameter count under a class-qualified name, so they can find the methods
that need review, such as oversized Flutter `build` methods. This closes the
request behind issue #325 and extends Lizard's many-language promise to one
more mainstream language.

**Scope:**

Required behavior:

- `.dart` files are discovered by default, selectable with `-l dart`, listed
  in the `-l` help, and analyzable through `analyze_file` and
  `analyze_source_code`. The README language list gains Dart.
- Function boundaries: top-level functions; members of classes, mixins,
  extensions, and enums: methods, getters (`get name`), setters
  (`set name(v)`), and constructors with a body (unnamed, named, and
  `factory`). Block bodies and `=>` expression bodies both form a function.
  `async`, `async*`, and `sync*` between the parameter list and the body do
  not break recognition. A declaration with no body, such as
  `Point(this.x, this.y);` or `Point.origin() : x = 0, y = 0;`, is not a
  function, following Lizard's existing rule that a function needs a body.
- Names: top-level `name`; members `Class::member`; unnamed constructor
  `Point::Point`; named and factory constructors `Point::fromJson`; getter
  `Point::sum`; setter `Point::scale=` (Dart's own name for a setter).
- Decisions (CCN): `if`, `for`, `while` (including `do … while`), `case`,
  each exception clause once whether written `on T`, `catch (e)`, or
  `on T catch (e)`, `&&`, `||`, `??` and `??=`, and the ternary `?`. In a
  switch expression each arm counts once except a bare `_` wildcard arm,
  matching `case` versus `default`. Not decisions: nullable `T?`, `?.`,
  `?..`, `!`, `is`, `as`, `await`, `else`, `default`.
- Strings and comments: `'…'`, `"…"`, `'''…'''`, `"""…"""`, raw `r'…'`,
  `//`, `/* … */`, and `///` doc comments. Their contents introduce neither
  decisions nor functions; comment lines are excluded from NLOC.
- Closures: a function literal's lines, tokens, and decisions belong to the
  enclosing function. Closures are not reported separately.
- Parameters: positional, optional positional `[…]`, and named `{…}`
  parameters each count one per declared parameter; `this.x` initializing
  formals count. Defaults and `required` add nothing.
- Existing language analysis is unchanged: the full suite stays green.

Deferred promises (not built or verified by this story):

- Decisions inside string interpolation `${…}` are not counted. JS/TS count
  them since SEED-019; Dart can follow later if wanted.
- Reporting closures and function literals as separate `(anonymous)`
  functions, as the JavaScript and Kotlin readers do. The enclosing-function
  rule was chosen because it is the smaller build and keeps a Flutter `build`
  method's complexity visible in one place; the developer may reverse this in
  a follow-up.
- Extension metrics beyond the core four (nesting depth, and so on) are
  whatever the shared machinery yields; not verified here.
- `if (x case pattern)`, records, extension types, and other Dart 3 forms not
  in the key examples follow the common rule without their own verification.

Rejection constraints: none. No product requirement calls for rejecting any
Dart input.

Boundary assumptions:

- Dart 3 syntax (sound null safety, patterns, switch expressions, class
  modifiers) is the target; legacy Dart 2 code parses under the same rules.
- Lizard depends on no external Dart toolchain, consistent with every
  existing reader.
- An unformatted ternary `a?b:c` without spaces reads as a nullable and is not
  counted; this limit is shared with the Kotlin and Swift readers.

**Key examples:**

1. **Discovery and selection.** A directory holds `lib/main.dart` and
   `tool.py` → `lizard .` → both files' functions are reported; `lizard -l
   dart .` reports only the Dart functions; the `-l` help text lists `dart`.
2. **Straight-line and branching.** `String greet(String name, {required int
   times, bool shout = false}) { var s = 'Hello $name ${shout ? "!" : "."}';
   if (times > 1 && shout) { s = s.toUpperCase(); } return s; }` → one
   function `greet`, CCN 3 (`if`, `&&`; the ternary inside `${…}` is not
   counted), 3 parameters, NLOC 7 when laid out one statement per line.
3. **Expression body with null-aware operators.** `int? maybe(int? x, [int
   fallback = 0]) => x ?? fallback;` → one function `maybe`, CCN 2, 2
   parameters, NLOC 1. The two `int?` add nothing.
4. **Class members.** A `class Point` with `Point(this.x, this.y);`,
   `Point.origin() : x = 0, y = 0;`, `factory Point.fromJson(Map<String,
   dynamic> json) { if (json['x'] == null) { return Point.origin(); } return
   Point(json['x'] as int, json['y'] as int); }`, `int get sum => x + y;`,
   `set scale(int f) { if (f > 0) { print(f); } }`, `bool get isOrigin {
   return x == 0 && y == 0; }`, and `@override String toString() =>
   'Point($x, $y)';` → functions `Point::fromJson` (CCN 2, 1 parameter),
   `Point::sum` (CCN 1, NLOC 1), `Point::scale=` (CCN 2), `Point::isOrigin`
   (CCN 2), `Point::toString` (CCN 1); the two body-less constructors are
   not reported. `Point.checked(this.x, this.y) { if (x < 0) { throw
   ArgumentError(); } }` → `Point::checked`, CCN 2, 2 parameters, and an
   unnamed `Point(this.x, this.y) { assert(x >= 0); }` → `Point::Point`.
5. **Switch statement and switch expression.** `String classify(int n) {
   switch (n) { case 0: return 'zero'; case 1: case 2: return 'small';
   default: return 'big'; } }` → CCN 4. `String size(int n) => switch (n) {
   0 => 'zero', 1 || 2 => 'small', _ => 'big' };` → one function `size`,
   CCN 4 (two counted arms plus `||`; `_` adds nothing), and no
   `(anonymous)` function from the arm arrows.
6. **Exception clauses.** `try { a(); } on FormatException { b(); } on
   IOException catch (e) { c(); } catch (e) { d(); }` inside a function →
   that function's CCN is 4: each of the three clauses counts once.
7. **Flutter build method with closures and generators.** `Widget
   build(BuildContext context) { return ElevatedButton(onPressed: () { if
   (enabled) { run(); } }, child: items.isEmpty ? Text('none') :
   Text('${items.length}')); }` inside `class MyWidget` → one function
   `MyWidget::build`, CCN 3, and no `(anonymous)` function. `Stream<int>
   gen() async* { yield 1; }` followed by `int after() => 1;` → both `gen`
   and `after` are reported.
8. **Strings and comments stay inert.** A function containing `/// if while`
   above it, `var r = r'$x ? y : z';`, and `var m = '''it's ? a : b''';`
   → CCN 1; the doc comment line is excluded from NLOC and the following
   function is still recognised.
9. **Other languages unchanged.** Running the full test suite after the
   reader lands → green, with no change to any existing language's results.
