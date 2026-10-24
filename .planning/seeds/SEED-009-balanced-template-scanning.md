# Balanced JavaScript and TypeScript templates

<a id="balanced-template-scanning"></a>

### Restore function boundaries with balanced template scanning

**Identity:** SEED-009#balanced-template-scanning
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Fix the nested-template regression with balanced scanning, add tests
for deeper nesting, quotes and escapes, and verify correctness and performance.

Scope: replace the limited template regex and keep template recognition and
splitting coherent across JavaScript, TypeScript, JSX and TSX. Preserve the
existing return-annotation fix, regex literal handling, normal templates,
function ranges, NLOC and following-function discovery. Do not introduce an
arbitrary nesting limit or expand unrelated parsing responsibilities.

The valid JavaScript regression has `function a(x)` on lines 1–3 returning
`` `${`${x ? `${`q`}` : ""}`}` `` on line 2, a top-level `const` on line 4,
and `function b()` on lines 5–7. Expected: `a@1–3` and `b@5–7`, both NLOC 3.
Current JS/TS results: only `a@1–2`, NLOC 2; `b` is missing. The prior review
validated syntax with Node and isolated the regression to the template regex.

Required proof: reproduce before repair through `analyze_file`; cover multiple
nesting depths, quotes/backticks/braces inside strings, escaped delimiters,
escaped interpolation, and following functions; preserve existing reader
tests. Recheck the issue #521 examples through the CLI, run full project checks,
review code quality independently, and compare bounded performance measurements
against the current implementation. Use the bug-fixing skill's bounded
planless attempt with no replanning; remaining uncertainty is scanner reuse,
integration and performance, not the observed regression.
