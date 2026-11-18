# Return a short verdict over retained proof

## Write the full report and return the verdict

Before returning, write the full report in the [evidence location](proof-evidence.md#evidence-location).
The full report carries every requirement in the targeted-return section below,
including all proof detail and the consumer-trace record. Keep the existing
substantive evidence; a short verdict does not replace it.

Return a short verdict containing:

- each slice promise with its observation location and result, or its named gap;
- the changed paths;
- the [run records](proof-evidence.md#read-the-run-record);
- stops and consequential learnings; and
- the full report's path in the evidence location.

Use the requirements below for that full report on disk. The coordinator uses
the verdict to locate the evidence and applies [proof acceptance](wrap-up.md#accept-proof).

## Return a targeted report with focused proof

Require uncommitted changes with passing focused proof, a stop requiring human
judgment, or an oversized-slice report under execution decisions. Require a
targeted return that gives the coordinator:

- the implemented outcome mapped to the slice promises;
- owned changed paths and the product or behavior boundaries they change;
- literal proof commands and concrete observation locations, including the
  relevant setup and assertions or signals;
- when filtered proof was used, which tests or observations were actually
  selected versus the promises claimed as covered;
- for each behavior presented as verified or covered, the observing assertion
  or signal that exercises it — or an explicit untested / uncovered listing
  instead of verified prose;
- when the change altered an operation or contract, a message or value, a
  default, rendered output, or shared text: the changed effect and its
  [consumer-trace record](../../dough-story-refinement/references/executable-proof.md#record-the-consumer-trace)
  with its supplied effect terms and each affected purpose's boundary, proof
  type, observation or gap under that common rule; link the inspected setup and
  observing assertions for proof run or reused, and support exclusions;
- required observations still missing or explicitly untested, named as gaps —
  a learning note alone is not acceptance evidence for those promises;
- for expensive proof, the retained or reconsidered decision and any deferred
  required observation with its owner and next condition;
- uncovered promises, contradictions, and other evidence gaps; and
- only consequential learnings that affect acceptance or remaining work.

Use source paths plus named tests, symbols, assertions, or signals as locations;
include a bounded excerpt only when the location cannot expose the decisive
evidence. The coordinator inspects the returned work and decides slice completion
under [proof acceptance](wrap-up.md#accept-proof).

For each passing focused command, use:

```text
proof:
  command: <literal complete focused command>
  covers: <observable behavior or paths covered>
  boundary: <product boundary exercised>
  observations:
    - <source/test path and named assertion or signal>: <what it observes>
  setup: <source/test path and setup supplying only the starting precondition>
  result: pass
```

Connect proof to the planned slice's, quick story's, or instruction's promises. Placeholders,
abbreviations, and paraphrases are ambiguous evidence. When no setup is needed,
say `none`; do not omit the field or mistake behavior supplied by a fixture for
product behavior. Report uncovered behavior as incomplete implementation; the
refactor pass must not supply missing behavior.

The `proof:` block is an example representation, not a required schema. An
equivalent layout that carries the same substantive evidence — the literal
complete command, what it covers, the boundary exercised, concrete observation
locations with their setup, and the result — is a complete report. Headings,
key casing, and field arrangement may vary; the substance may not. A report
missing that substance, or contradicting observed evidence, is incomplete
however closely it matches the example. The coordinator's
[proof acceptance](wrap-up.md#accept-proof) applies the same substance test.

Explicit completion markers with a separate workflow contract — such as
`## REFACTOR COMPLETE` and `## PAUSED FOR CI` — are not report formatting;
return them verbatim.
