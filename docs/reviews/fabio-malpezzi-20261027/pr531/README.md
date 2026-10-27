# PR531 retained review inputs

Disposition: defer the real defect to
[SEED-013#arrow-object-boundaries](../../../../.planning/seeds/SEED-013-arrow-object-boundaries.md#arrow-object-boundaries)
and leave authored head `47aa35735cccba962e507edb179eae1154ab6ad2` unmerged.
[review-findings.md](review-findings.md) owns observations, commands and limits.

The one checked-in performance harness is [../paired.py](../paired.py), SHA256
`3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
The executed external copy has identical bytes. Copy that harness beside this
README for replay; no second implementation is maintained in this directory.

Reconstruct the 100 source inputs from manifest.revision using the parent
README's git-show/hash recipe, skipping affected/ entries in that first loop.
Write every filename/source from inputs.json under corpus/affected and verify
each SHA256 against manifest.json. There are 64 affected inputs and 164 hashes.
The API workload analyzes those 64 related cases, including four real Vue
script blocks. The CLI workload analyzes only the four original source targets.
No checked-in corpus or changing checkout is used as timed input.

Use independent source snapshots at the recorded target and authored head,
the same Nix interpreter and the literal commands in review-findings.md.
Default-path equivalence and its ENS limitation are in comparison-basis.json.
The metadata, complete patch, explicit expected analyzer summaries, terminal
reproduction logs, raw pairs, profile and proof setup are retained here.

The raw proposal has new correctness regressions and an inconclusive affected
performance gate. Neither the broader CLI pass nor the existing focused-suite
pass qualifies a future corrected candidate. There was no hosted write.

Terminal log copies have trailing whitespace normalized for Git; the original
external logs and every observation are retained unchanged in substance.
