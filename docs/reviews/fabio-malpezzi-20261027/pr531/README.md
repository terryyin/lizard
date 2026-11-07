# PR531 retained review inputs

Disposition: the deferred defect was corrected and merged through
[PR531](https://github.com/terryyin/lizard/pull/531); see
[review-findings.md](review-findings.md) for observations, commands, limits,
and the delivered head. Authored head `47aa35735cccba962e507edb179eae1154ab6ad2`
was the review baseline before correction.

The original measured performance harness has SHA256
`3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
The executed external copy has those bytes. Recover that immutable version
with the parent README's historical-harness recipe beside the reconstructed
corpus; the current central harness has since changed. No duplicate is checked in.

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
