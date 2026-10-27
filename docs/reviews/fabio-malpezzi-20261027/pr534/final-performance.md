# Exact committed-revision qualification

Candidate: `ec2ec81565e037883066f220ab794f912b8df2b8`.
Current target: `34cd43ddd1915438f3b440088672bfe4a08d96bf`.
Cumulative baseline: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
Earlier initial/8dac experiments and all profiles remain unchanged. This
record covers the separately labelled exact candidate after PR536 integration.

## Prospective selection and resource decision

Keep the original 128-input affected API gates versus current and frozen, and
the unchanged 100-file broad CLI gates versus both. Freeze all 38 subsequently
added query/trivia controls from the explicit public test cases: four runtime
assignment forms × four JS/TS/JSX/TSX consumers × two alternate forms, plus
typed/empty parameter trivia in TS/TSX/Vue. Their expected initializer function
is the accepted current-target behavior. The augmented 166-input API must also
pass versus current and frozen. All six upper paired 95% slowdown bounds must
be strictly below 3%; additions and broad results cannot offset original failure.

Every comparison is fixed before timing: unchanged original `paired.py`, 20
alternating warmed pairs, 1.5-second calibration and every measured batch over
one second. No favorable retry or statistical protocol change is authorized.
The literal original manifest/harness hashes are retained alongside separate
complete corpus manifests for the original and augmented selections.

Before setup, Territory.local had 16 CPUs/48 GiB, load 6.35/8.16/7.76, about
214,440 free and 848,753 inactive 16-KiB pages. A native VM used about seven
CPU cores; its activity and other native worktrees are outside this execution's
ownership. The coordinator confirmed no other owned heavy proof; the ordinary
CI observer remains active. Six serial comparisons require one active timed
analyzer/CLI process (the API keeps two alternating workers), modest memory,
and an estimated seven to ten minutes plus setup. Available CPU/memory supports
this bounded launch. The measurement driver records a fresh process/headroom
observation before starting. Changed contention, failures or unexplained noise
require reassessment; a passing-looking retry is not evidence.

## Corrected setup fixture

The first freeze command terminated 1 before any timing. Its CLI expectation
incorrectly reused the historical output hash containing the old absolute
corpus prefix. A bounded byte comparison proves the complete candidate output
changes only by the exact frozen corpus-prefix substitution. Its failed freeze,
raw output and numeric terminal receipt remain in `seed015-final-ec2ec815`.

The corrected freeze is separately labelled
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-final-ec2ec815-setup2`.
It changes no source, harness, replay input or timing protocol. All three actual
CLI outputs use the same new frozen corpus and are compared byte-for-byte;
their actual new hash is retained. The old hash is only a historical reference,
with the exact output-prefix correspondence checked independently.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/final-qualification-freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/final-qualification-measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/final-qualification-postcheck.py
```

The corrected freeze terminated 0. It archives exact Git revisions without
copying a mutable installation, asserts actual facade/registry/reader/state/
core/helper imports before and after collection, and observes 166 candidate
expected public rows with independent FileAnalyzer agreement. All seven public
fields of the original 128 rows match the accepted integration replay. The
candidate's 104 product source hashes match the accepted working-product map.
Baselines' semantic differences remain comparison context. Original corpus
bytes and all old manifests/results are preserved; only labelled supplemental
files are added to the second corpus.

Each subprocess writes a separate log and explicit numeric return-code receipt.
Verbose immutable-harness output goes to retained files; only compact progress
is streamed. The driver owns every child through exit and performs source,
corpus, import/public-result and full CLI checks after all timing commands.

## Results and terminal receipts

| Comparison | Median slowdown | Paired 95% interval | Minimum batch | Gate |
| --- | ---: | --- | ---: | --- |
| Original 128 API / current | +5.970% | [+4.674%, +6.891%] | 1.514s | Fails |
| Original broad CLI / current | −0.603% | [−0.922%, +0.214%] | 1.541s | Passes |
| Original 128 API / frozen | −3.797% | [−4.210%, −3.489%] | 1.460s | Passes |
| Original broad CLI / frozen | −0.251% | [−0.616%, +0.292%] | 1.528s | Passes |
| Augmented 166 API / current | +6.755% | [+5.581%, +7.259%] | 1.527s | Fails |
| Augmented 166 API / frozen | −1.128% | [−1.618%, −0.879%] | 1.455s | Passes |

Each distinct comparison ran once with 20 warmed alternating pairs. Every
child harness command terminated 0; corrected freeze, measurement driver and
postcheck also terminated 0. These exits establish complete observations,
not a passing performance verdict. Both current-target API intervals are
entirely above 3%; this candidate is **not performance-qualified**. No retry,
profile, runtime optimization or gate waiver followed these measurements.

`final-{original,augmented}-{current,frozen}-api.json` and
`final-original-{current,frozen}-cli.json` retain every pilot, loop count,
raw duration, median, variability and paired interval. `final-freeze.json`
retains exact revisions, all four driver hashes, literal gate selection and
input hashes; separate affected/broad/source maps retain complete bytes.
The historical immutable manifest hash still describes its original corpus;
the two complete new manifests additionally cover every labelled supplement.

Before and after timing, all 166 candidate expected rows pass with independent
FileAnalyzer agreement and asserted actual imports. Every public field remains
identical to the corresponding pre-timing observation; original 128 seven-field
values still match the accepted integration replay. Source/corpus/harness
hashes remain unchanged. All three new-corpus CLI outputs are byte-identical,
warning exit 1, actual SHA256
`3adeb4d133e695385615a788f09fa35b31e28ae888e6badfae10f129eb545e8a`.
Only the absolute corpus prefix differs from the historical output; no replay
source, harness or timing was normalized or rewritten.

`final-postcheck.json` indexes the numeric per-process receipts and evidence
hashes. Complete logs, receipts and pre/post metrics remain in the setup2
cache named above. `final-{freeze,measure}-driver-receipt.json` retain the
successful driver exits, additionally observed as numeric terminal tool exits.
The failed first setup is retained in `final-setup-failure.json`; its four
original driver source files were reconstructed in its cache and verified
byte-for-byte against all four hashes frozen before that setup failure.

G4 remains unresolved. G6's new-run terminal observations are complete while
the historical loss remains explicit. Refactoring/delivery and any subsequent
coherent optimization remain with the coordinator; no plan status, source or
test was changed by this qualification. All owned commands are terminal and
no extra verification watch was created.
