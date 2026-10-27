# PR533 retained review evidence

Disposition: defer to [SEED-014#multiline-arrow-boundaries](../../../../.planning/seeds/SEED-014-multiline-arrow-boundaries.md#multiline-arrow-boundaries)
and leave authored head `15423852c04aed302f684395fe9522251d88a2f2` unmerged.
[review-findings.md](review-findings.md) owns the observations and limits.

The single checked-in performance harness is [../paired.py](../paired.py),
SHA256 `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`.
Copy its identical bytes beside the reconstructed corpus for execution.
No changing checkout is used as timed input.

Reconstruct the 100 original source inputs with the parent README's
git-show/hash recipe using this manifest, skipping affected/ entries. Write
each inputs.json filename/source as corpus/affected/<filename>. Verify all
250 SHA256 values in manifest.json. The affected workload has 150 inputs;
the broader CLI analyzes only the original four source targets. Expected
metrics may be corrected independently of these frozen source bytes.

Create independent source snapshots using git archive at target7ac449f and
baseline2e21e404. To create raw, copy target and git-apply the complete diff
from proposed-patch.json in that copy. The diagnostic child starts from raw
and applies independent-child-patch.json; it removes only the object-opening
heuristic/check and is evidence, not an accepted correction.

Copy reproduce.py, authored-tests.py, profile-control.py, inputs.json and
manifest.json beside the external corpus and identical paired.py. The original
five child-test methods in authored-tests.py are extracted byte-for-byte from
the frozen head's class. Reproduce imports assert the facade, registry, function
owner, TS states and test helper all come from the selected snapshot. Use a
real script here: python -c from a different checkout can select wrong readers
because lizard.py prepends sys.argv[0]'s directory.

Use one Nix entry at a time from the prepared execution checkout. Commands,
snapshot definitions and terminal outcomes are in review-findings.md. Default
target/baseline equivalence is in comparison-basis.json; no ENS claim is made.
Historical four-job CI success does not qualify the current fourteen-job gate.

The first harness attempt omitted get_extensions([])'s required argument;
its failed diagnostic is retained. Initial fixed-count expectations included
two extra signature tokens and were corrected to the existing public convention
(4 with semicolon, 3 without); source bytes never changed. A first attempted
target test-class selection could not find a class introduced only in the PR;
the unchanged extracted class supplies the proper red proof. No failed
diagnostic qualifies behavior, and no production assertion was weakened.

Terminal copies have trailing whitespace normalized for Git. Original external
logs remain under the cache root recorded by the performance results.
