# PR551 class methods after semicolon-less fields

**Disposition:** merge-ready at corrected head
`b0733889d075dbbbfa2985648e5910e383dd36aa` (frozen contributor head
`a8dabb2351014cbf5164fe31a3472036877130fc` plus hot-path guards). Target /
frozen baseline: `4a05e4056cebb2edd75cb02b7a43e911369f4ce5`.

## Value

On current target, a class field without `;` followed by `async` / `static` /
`get` / `set` reports no methods — the open field value swallows them. Correct
behavior reports those methods with proper boundaries in `.js` and `.ts`. A
multiline `handler =` / `async () => {}` field stays one field.

## Adaptation

Contributor patch applies to current target. Adds `_end_field_before_modifier`
on `JavaScriptObjectsMixin` and calls it from `get`/`set` and from
`TypeScriptStates` before `static`/`async`. Maintainer follow-up guards those
calls with `if self._in_prop_value` so common modifier tokens skip the helper
while the semicolon-less field case still ends the field first. Owners remain
`javascript_objects` / `typescript_states`.

Independent refactor: none beyond that hot-path guard (required for the
performance bound).

## Proof

```sh
PIP_QUIET=3 nix develop -c python docs/reviews/fabio-malpezzi-SEED-034/pr551/reproduce.py
PIP_QUIET=3 nix develop -c python -m pytest \
  test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py \
  test/test_languages/testES6.py test/test_languages/testJSX.py \
  test/test_languages/testTSX.py test/test_languages/testVue.py \
  test/test_languages/testOptionalOperators.py \
  test/testJavaScriptNestedStructures.py
PIP_QUIET=3 nix develop -c python -m pytest
PIP_QUIET=3 nix develop -c make
```

- Target reproduction: claim fixtures red; control green.
- Corrected: all claim + control green.
- Focused: 1385 passed, 5 skipped.
- Full pytest: 3021 passed, 7 skipped.
- make: exit 0.
- Consumers: `consumer-trace-dispositions.md`.

## Performance

Attempt5 (unguarded helper) failed incremental API: change ~2.64%, CI upper
~3.28% on a quiet host — real overhead, identical function counts. Attempt6
after `_in_prop_value` call-site guards: all api / api-parameters / cli gates
pass; worst upper CI **0.989%**. Cumulative mirrors incremental (frozen trunk
equals current target). See `measurement-decision.json` and `attempt6/`.
