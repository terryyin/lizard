---
id: SEED-055
status: active
planted: 2026-11-11
scope: story
---

# Report line counts for Gherkin feature files

<a id="gherkin-support"></a>

### Report line counts for Gherkin feature files

**Identity:** SEED-055#gherkin-support
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Goal.** Teams that maintain Gherkin specifications can use Lizard to see how
long each scenario and shared setup block is in their `.feature` files.

**Scope.** Report one Lizard function for each written `Background`,
`Scenario`, and `Scenario Outline`. Count source lines using Lizard's NLOC
convention within each function's own block. Every reported function has
CCN `1`.

**Key example.** A feature file with one `Background`, one `Scenario`, and one
`Scenario Outline` with two `Examples` data rows produces three function
results. The background's lines count toward its own NLOC, and the outline's
examples belong to its single result.
