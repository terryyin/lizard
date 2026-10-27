# PR528 retained review inputs

Disposition: defer to SEED-012#regex-literal-context; leave authored head
b66ec3a7f3ca1b3f59d8a76891ce556406231566 unmerged. See review-findings.md for
commands, expected/actual observations, shared consumers and performance verdicts.

The unchanged literal harness is ../paired.py (SHA256
3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5).
The executed external copy had that exact content. To replay its fixed root,
copy that harness into this directory before invoking it; no second checked-in
implementation is maintained.

Reconstruct100source inputs from manifest.revision using the parent README's
git-show/hash recipe. Reconstruct the49affected inputs from inputs.json, writing
each filename/source pair under corpus/affected and checking manifest hashes.
The affected API analyzes only these related cases; the broader CLI analyzes
the unchanged lizard.py/lizard_core/lizard_languages/lizard_ext source trees.
Keep the same interpreter and input bytes for both source checkouts.

proposed-patch.json and review-input.json preserve the complete proposed change
and metadata/discussion/check snapshot. reproduce.py and its terminal logs expose
both the existing report and the newly introduced valid-division regressions.
profile-control.py and its bounded outputs explain the sampled lexical cost with
identical public metrics. Raw result JSON retains all20timing pairs and intervals.

These timings diagnose the unmergeable raw proposal. They do not qualify any
future corrected revision or establish separate Vue/ENS performance.
