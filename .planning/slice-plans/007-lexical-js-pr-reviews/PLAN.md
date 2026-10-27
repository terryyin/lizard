# Lexical/condition JS/TS PR reviews

**Identity:** SEED-010#review-lexical-js-prs
**Source:** [canonical story](../../seeds/SEED-010-fabio-malpezzi-pr-reviews.md#review-lexical-js-prs)

## Outcome and boundaries

JS-family users and maintainers receive a complete disposition for each of
frozen PRs #544–#547: a worthwhile, fully verified merge with acknowledgement,
or an evidence-backed unnecessary/inadequate conclusion with any >15-minute
follow-up linked. Comment termination and condition counts match the language
rules those PRs claim when a change is accepted.

Exactly those four PRs from the frozen set and story map. Refresh each
selected state/head and the current target before acting. Later arrivals and
other authors are excluded. Each PR owns its entire
[complete-cycle contract](../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md)
in its slice — reproduction/value, whole-concept and shared-language consumers,
eligible ≤15-minute correction, fresh independent refactor, affected/broad
incremental and cumulative performance strictly below 3%, full suite /
format / required CI, exact-head merge and thanks — or the documented
non-merge disposition. No phase is deferred to a sibling story.

Deferred: structural PRs #537–#543; -ENS keyword-member counting; residual
keyword-member-as-condition inside template interpolations after both #545 and
#546 land; C/C++ `\` line-comment continuation; optional chaining / nullish
token shapes beyond #547's discrimination; new languages or options. No
supplier agreement with the structural sibling. If this story finishes last
among remaining review batches, it also reconciles the full 19-PR cohort and
combined evidence without waiving any individual pre-merge performance gate.

This preparation authorizes no Take or execution.

## Existing owners and direction

PFE: change the existing JS-family lexical owners rather than add parallel
parsers. `TypeScriptReader` in `lizard_languages/typescript.py` (with
`CCppCommentsMixin`) is the shared reader for `.js`/`.ts`/`.jsx`/`.tsx`
condition and token behavior; `lizard_languages/javascript_templates.py`
owns template scanning and line-comment patterns used inside interpolations;
`lizard_languages/tsx.py` owns the TSX tokenizer path that currently lacks the
optional-marker rule #547 unifies. Preserve installed consumer traces and
Accepted [ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md) /
[ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
terminology (reader, CCN, NLOC, condition). Index and records agree; no new
architecture decision or North Star topic is warranted. At execution, run the
installed consumer trace against each actual diff.

## Observed premises

Observed on 2026-10-28 in
`/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a`,
product revision `d6be477c37eea80bdf1423ed35e15338db398968`.

- Public `lizard.analyze_file.analyze_source_code` on current target still
  matches contributor baselines: `b` CCN 3 / NLOC 7 after `// \`; `label`
  CCN 1 for interpolation conditions; `load` CCN 2 for `.catch`; `pick`
  CCN 1 for `x?y:0`; TSX `m` CCN 3 with optional field/parameters.
- Frozen heads still equal open PR heads (`3ac7e4ea…`, `baa95a4d…`,
  `a6716321…`, `27665570…`); each PR is open with hosted checks SUCCESS.
- Contributor regression class names are absent on current target; they arrive
  with each PR head. Discriminating proof for each slice is public analysis
  red→green plus the PR's tests once evaluated on that head/candidate.
- Contract focused suite route is available here:
  `nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py`
  → **641 passed, 5 skipped** (also ran `testOptionalOperators.py` with that set).

No scratch product edits remain. Premises above settle defect presence and
proof-route availability for all four slices; per-PR head refresh, value
judgment, correction eligibility, performance, and merge remain execution work
inside each slice's complete cycle.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| #544 line-comment termination | Slice 1: public analysis CCN/NLOC on the `b` fixture across four extensions; C/C++ continuation control if shared comment code changes; focused JS/TS suite + full/make/CI |
| #545 interpolation conditions | Slice 2: public analysis CCN on `label`; boundary/nesting/regex/literal controls; focused suite + full/make/CI |
| #546 keyword member names | Slice 3: public analysis CCN on `load` and genuine control-flow controls; focused suite + full/make/CI |
| #547 unspaced ternary vs optional | Slice 4: public analysis on `pick` and TSX/JSX optional field/params; `?.` / `??` controls; focused suite including `testOptionalOperators.py` + full/make/CI |
| Performance & disposition | Each slice: contract performance protocol and merge/thanks or evidenced non-merge record |

Follow [AGENTS.md](../../../AGENTS.md) and the review contract. Mock only
filesystem boundaries. No project numeric slice limit was supplied; each slice
is one complete review loop sized by the Behavior/Structure gate.

Detail: [SLICE-PROOF.md](SLICE-PROOF.md).

## Ordered slices

### 1. JavaScript line-comment termination — PR #544
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/544
Frozen head: `3ac7e4eaf61e898836fded88ab715b37f9fedbbf`
Proof: [slice 1](SLICE-PROOF.md); complete-cycle gates owned here.

Behavior: given JS-family source whose line comment ends with `\`, analyzing it
does not hide the next line's tokens from CCN/NLOC; C/C++ continuation after
`\` is preserved when shared comment machinery is touched; the PR receives a
full contract disposition.

### 2. Conditions in template interpolations — PR #545
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/545
Frozen head: `baa95a4df8278421bb3d3a6487516030cc03a979`
Proof: [slice 2](SLICE-PROOF.md); complete-cycle gates owned here.

Behavior: given a template whose interpolations contain conditions, analyzing
the four extensions counts those conditions in CCN while preserving function
boundaries, state tokens, nested templates, and non-code lexical contexts; the
PR receives a full contract disposition.

### 3. Keyword member names — PR #546
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/546
Frozen head: `a671632141127fca9b2785fdbb0c675d45ae9042`
Proof: [slice 3](SLICE-PROOF.md); complete-cycle gates owned here.

Behavior: given member access named `catch`/`if`/`for`/`while`/`case` (including
`?.`), analyzing the four extensions does not add conditions, while genuine
control-flow constructs still do; the PR receives a full contract disposition.

### 4. Ternary and optional markers — PR #547
Type: Behavior
Status: planned
Source: https://github.com/terryyin/lizard/pull/547
Frozen head: `2766557096daac3f8e55657e9c48ba685157635d`
Proof: [slice 4](SLICE-PROOF.md); complete-cycle gates owned here.

Behavior: given an unspaced ternary, analyzing `.js`/`.ts` counts it; given
optional fields/parameters in `.tsx`/`.jsx`, analyzing does not count those
markers as ternaries; `?.` and `??` keep existing behavior; the PR receives a
full contract disposition.

## Cumulative design

One coherent model: JS-family lexical context and condition counting owned by
the shared TypeScript reader and its template/TSX token helpers. Each slice
corrects a distinct misclassification (comment end, interpolation opacity,
member-name keywords, optional vs ternary) without a parallel condition
counter. The #545+#546 interpolation residual stays a deferred follow-up, not a
fifth recognizer in this plan.

## Concern evidence

Slice-plan refinement was not needed: four independent Behavior slices map
1:1 to the refined examples, proof ownership is named, decisive target
premises are observed in this checkout, and no remaining slice-boundary,
sizing, or cumulative-design concern blocks readiness.
