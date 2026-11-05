---
id: SEED-037
status: active
planted: 2026-11-06
planted_during: GitHub issue 561, C# methods after a nested class
scope: C# reader class context for methods declared after a nested class
---

# SEED-037: C# methods after a nested class

## Stories

<a id="csharp-methods-after-nested-class"></a>

### 1. Report C# methods after a nested class under the outer class

**Identity:** SEED-037#csharp-methods-after-nested-class
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** Resolve the reported discrepancy: a C# method declared in an outer
class after a nested class is reported under the outer class.

**Scope:** The C# reader's class context for methods that follow a nested
class in the same outer class. Leave other languages and unrelated C# metrics
unchanged.

**Expectations:**

- Expected: `python lizard.py -l csharp` on the issue's `Repro.cs` reports
  `Outer::Method` at lines 8–10.
- Actual (reported on 1.25.0): the same method is reported as
  `Inner::Method` at lines 8–10. After the nested class closes, its name
  stays the current class, so later methods in the outer class take the
  nested class's name.
- Evidence: GitHub issue 561, reporter vacavaca, opened 2026-11-05. No
  comments. The report includes the source and the command.
- Qualification: before final delivery and closure, qualify the entire
  repair with `lizard-process-pr --local-change`. The issue source is
  https://github.com/terryyin/lizard/issues/561. Starting revision is the
  trunk revision this admission publishes from.
