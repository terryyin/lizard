---
id: SEED-052
status: active
planted: 2026-11-09
scope: story
---

# See Lizard test case counts in Intent

<a id="publish-explicit-ci-test-counts"></a>

### See Lizard test case counts in Intent

**Identity:** SEED-052#publish-explicit-ci-test-counts
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Goal.** Lizard maintainers can see how many analyzer test cases and website
scenarios the selected CI attempt recorded in Intent.

**Scope.** Adopt the new format in the existing CI proof publisher for the representative
analyzer pytest leg and the website Cucumber job. Preserve suite locations,
outcomes, file breakdowns, and available execution times.

Adopt once Open Dough publishes the explicit test case and test file units
and Intent can read the new format. See the [Open Dough protocol story](https://github.com/terryyin/open-dough/blob/b8fdb3a361899488e12828383afdea02cb545fda/.planning/seeds/SEED-281-distinguish-test-cases-and-files-in-ci-proof.md#distinguish-test-cases-and-files-in-ci-proof).

**Key examples.**

- The new summary reports one test case per pytest result or Cucumber
  scenario result. For the same CI attempt, Intent and both runner reports
  agree on the case totals and outcomes.
