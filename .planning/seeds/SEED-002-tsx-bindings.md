---
id: SEED-002
status: active
planted: 2026-10-22
planted_during: Investigation of GitHub issue 523
trigger_when: Analyzing TSX JSX attributes containing conditional expressions
scope: small
---

# SEED-002: TSX binding candidate lifetime

<a id="analyze-conditional-style-arrays"></a>

### Analyze TSX conditional style arrays without crashing

**Identity:** SEED-002#analyze-conditional-style-arrays
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Lizard analyzes valid TSX style arrays with conditional elements,
including subsequent JSX child calls, without an exception or corrupted function
metrics.

#### Scope

- Investigate and repair the binding candidate lifecycle reported in
  https://github.com/terryyin/lizard/issues/523.
- Reproduce through `analyze_file.analyze_source_code` before repair; check
  equivalent parenthesized syntax and relevant JavaScript/TypeScript coverage.
- Keep the repair bounded to one ten-minute implementation attempt.

#### Key examples

```tsx
const Alert = () => { return (<View style={[{ elevation: 0.4 }, mobile ? a : b ]}><Text>{t("hello")}</Text></View>); };
```

The example should yield the Alert function and conditional complexity without
raising `IndexError`. Grouping `(mobile ? a : b)` should preserve its metrics.

#### Report and remaining uncertainty

The reporter observed Lizard 1.24.2 on Windows leaving a binding candidate active
with empty `_frames` and `_expression_brackets`; accessing `in_expression` on a
later `(` raises `IndexError`. Cause, scope, and introducing release remain to
be confirmed locally. An empty-stack guard alone may conceal incorrect state.
