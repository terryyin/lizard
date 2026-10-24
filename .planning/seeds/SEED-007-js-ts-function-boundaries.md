# JavaScript and TypeScript function boundaries

<a id="js-ts-function-boundaries"></a>

### JS/TS function boundaries

**Identity:** SEED-007#js-ts-function-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Resolve the two function-end discrepancies reported in
[issue #521](https://github.com/terryyin/lizard/issues/521) so JavaScript and
TypeScript users receive accurate function ranges and all following functions.

Scope: TypeScript return annotations and nested template literals inside
interpolations. Preserve existing regex literal, template, and function parsing.

Expected examples:

- `function a(): number { return 1; }`, spread across lines 1–3, ends on
  line 3 with NLOC 3; an intervening statement on line 4 is outside `a`, and
  `function b()` on lines 5–7 is reported with NLOC 3.
- Adjacent annotated functions have separate ranges; the first ends before
  the second declaration starts.
- A function on lines 1–3 returning `` `p ${x ? `q` : ""} r` `` ends on
  line 3 with NLOC 3, and a subsequent function on lines 5–7 is reported,
  for both JavaScript and TypeScript.

Reported actual behavior: the annotated function consumes the next statement
and loses `b`; the nested template makes `a` consume the rest of the file.
Reported on master ad778b0 and Lizard 1.24.1 under Python 3.14.3.

Uncertainty: reproduce on current trunk, identify the smallest bounded repair,
and verify related reader behavior. Bug-fixing execution is bounded to a
ten-minute planless repair attempt with no replanning, after investigation.
