# Performance qualification

Select workloads from the change and its consumers before timing. For the Fabio
cohort, retain the existing complete-cycle contract: every required workload's
paired 95% upper slowdown bound must be **strictly below 3%**, with incremental
and cumulative comparisons. A different story may specify a different policy;
record it rather than silently generalizing old constraints.

## Reuse the runner

`scripts/paired_benchmark.py` generalizes the retained cohort `paired.py` protocol.
It does not edit historical evidence or decide whether the candidate is correct.
Run semantic regressions first; a stable wrong output is still wrong.
The runner uses POSIX pipes and host-load observations; run it in the project's
Nix environment on Linux or macOS.

Prepare independent baseline/candidate source checkouts and a fixed directory
containing only source inputs. Historical corpora can be reconstructed with the
retained cohort README and manifests. Keep outputs outside that directory.

```sh
nix develop -c python .agents/skills/lizard-process-pr/scripts/paired_benchmark.py \
  --baseline /absolute/target --candidate /absolute/candidate \
  --corpus /absolute/frozen-corpus --workload api-parameters \
  --out /absolute/evidence/api-parameters.json
```

Modes: `api` analyzes preloaded sources, `api-parameters` also consumes public
parameter names/counts, `cli` runs the actual CLI on the fixed corpus. Supply
`--extension ns` when that extension is affected. The same extension selection
applies to both sides. CLI intentionally includes process/import/discovery cost;
API intentionally excludes setup and source reads. Neither replaces the other.

For noisier CLI work choose, prospectively, `--seconds 4`. Default: 20 alternating
pairs, batches calibrated to 1.2 seconds; measured batches shorter than one second
are inconclusive. More than 20 pairs uses the conservative t(19) multiplier.
Do not reduce the minimum to make a marginal result pass.

The helper records revisions, source/corpus/harness hashes, literal commands,
interpreter, load observations, module paths, stable pre/post observation hashes,
every duration, medians, standard deviations and paired log-ratio interval. It
checks inputs again after timing. Production hashes cover `lizard.py`,
`lizard_core/`, `lizard_languages/` and `lizard_ext/` Python files; if a PR affects
other runtime inputs, extend the measurement contract before relying on it.

API observation hashes include filename, function names, boundaries, NLOC, CCN,
token counts, parameters and counts. CLI hashes retain actual path-bearing output.
Baseline and candidate outputs may differ intentionally; only each side's own
pre/post stability is checked. Keep the expected-result proof separately.

The summary is one JSON object. Full evidence is on disk. `passes` is only this
measurement's numerical gate, not review approval or CI clearance. Errors retain
partial observations and return nonzero. Output files are never overwritten.

## Apply evidence without repeating work

- Freeze target/head and, for a cohort, its initial accepted-set baseline. Run
  candidate vs current target, then candidate vs initial baseline where required.
- Include all report/authored cases, nearby controls and shared consumers. Use an
  explicit broad frozen source corpus for CLI. Do not time only the changed syntax.
- Run one heavy comparison at a time, in one Nix environment. Inspect host load
  before launch; do not terminate unrelated user work to obtain a quiet machine.
- Interpret three states: passes; valid observation does not meet the bound;
  inconclusive/invalid setup. A failing bound does not establish causality. Profile
  the valid candidate to identify likely owners; profiling is not another gate.
- Keep earlier runs, including failures. Choose any longer/quiet retry before its
  launch and record the reason; do not discard unfavorable pairs.
- Reuse a result when production bytes, corpus, harness, interpreter/options,
  expected promises and consumers remain compatible. A head changing only owned
  acceptance notes can preserve timing; a changed consumer or dependency cannot
  be waved through merely because the head's diff looks small.
- Resolve every applicable affected gate before merge. Refresh current hosted CI
  on the exact final head independently of these local measurements.
