# Outside-in proof for SEED-034#process-fabio-malpezzi-prs

Preparation baselines on product revision
`5fbc4f64ec9d68790611833488eb30a8c76ec744`. Refresh each PR head and the current
target before relying on these figures. Claims are not automatic acceptance.

## Shared routes

- Public analysis: `analyze_file.analyze_source_code` / CLI; for #554 include
  the `ns` extension (`-ENS` / `get_extensions(['ns'])`), reading
  `max_nested_structures` (CLI NS column).
- Focused suite (complements full pytest / `make`):

```sh
nix develop -c python -m pytest \
  test/test_languages/testJavaScript*.py \
  test/test_languages/testTypeScript*.py \
  test/test_languages/testES6.py \
  test/test_languages/testJSX.py \
  test/test_languages/testTSX.py \
  test/test_languages/testVue.py \
  test/test_languages/testOptionalOperators.py \
  test/testJavaScriptNestedStructures.py
```

- Performance and disposition gates: previous-batch
  [REVIEW-CONTRACT.md](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md)
  and [lizard-process-pr performance](../../../.agents/skills/lizard-process-pr/references/performance.md).

## Slice 1 — PR #551

Fixture (JS; same shape in `.ts`):

```js
class Store {
  count = 0
  async load(x) { return x; }
  static create() { return new Store(); }
  get value() { return this.count; }
}
```

Current target: no functions reported. Expected after a worthwhile fix: `load`,
`create`, and `get value` present with correct boundaries. Control: multiline
`handler =` / `async () => {}` remains one field (already holds on current
target as `handler` lines 3–5).

Contributor tests (on PR head): `Test_JavaScript_member_modifier_after_a_field`,
`Test_TypeScript_member_modifier_after_a_field`.

## Slice 2 — PR #552

Fixture:

```ts
export function first<T>(list: T[]) {
  return list[0];
}
```

Current target: no function reported. Expected after a worthwhile fix: `first`
reported with parameters and end line; also cover `pair<T, U extends object>`.
Preserve already-reported generic methods and arrows.

Contributor test (on PR head): `test_generic_function_declaration`.

## Slice 3 — PR #553

Fixture:

```ts
function retryAfter(e: unknown) {
  const h = (e as { 'retry-after'?: string })['retry-after'];
  return h || '';
}
```

Current target: `retryAfter` CCN 3. Expected after a worthwhile fix: CCN 2
(only `||`). Control: `c ? 'a' : 'b'` / `c?'a':'b'` still add one decision.

Contributor test (on PR head): `test_optional_quoted_member_is_not_a_ternary`.

## Slice 4 — PR #554

Fixture:

```js
function load(p) {
  return p.then(() => 1).catch(() => null);
}
```

Current target with `ns`: both arrows reported; `.catch` callback has
`max_nested_structures=1`. Expected after a worthwhile fix: that callback NS 0;
real `if` inside a callback or `try`/`catch` still raises NS. Also cover
`?.catch`.

Contributor test (on PR head):
`test_member_named_like_a_structure_is_not_a_structure`.
