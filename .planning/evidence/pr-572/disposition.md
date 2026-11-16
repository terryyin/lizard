# PR #572 disposition

- **Decision:** merged
- **Merge:** `1ad4b1333c0b209bd45bed943ad0715189a23445` (2026-11-16)
- **Final head:** `0555c57345a5aa62118c8fa1fa2bcffb6463e749`
  (`ssenge:fix-ternary-between-comparisons`); PR base `153d629f`.
- **Owner:** SEED-059#process-pr-572
- **Corrections on contributor branch:** none.
- **Hosted CI:** "Python application" success on the final head after
  maintainer approval of the fork workflow
  (https://github.com/terryyin/lizard/actions/runs/43744401205): 16 build
  jobs (ubuntu-24.04 and windows-latest, Python 3.8–3.14), `changes` and
  `ci-proof` green; `web` skipped as not applicable.
- **Performance decision:** the maintainer accepted the result below as not
  material (2026-11-16); the strict 3% bound was not demonstrated.
- **Acknowledgement:** none posted (no corrections were made).

## Review

- **Reproduction:** trunk reports CCN 1 for `a < b ? c > d : e`; the PR
  reports 2. Confirmed through `analyze_source_code` for C/C++, Java, C#,
  ObjC, Dart, JS, JSX child expressions, TS, Swift, Ruby, Vue and Solidity.
  Nested `a < b ? c > d ? 1 : 2 : 3` gives 3.
- **Controls unchanged:** Java wildcards (`<?>`, `<? extends T>`,
  `<?, ? super T>`, nested `Class<? extends Enum<?>>`), nullable arguments
  (`List<int?>`, `Dictionary<string, object?>`, `Map<String, Any?>`,
  `Map<out K, V?>`), TS conditional types, JSX/TSX elements in ternaries,
  Kotlin `?:`/`?.`, Rust `?`, PHP (own `<` rule), parameter counts and
  following-function boundaries.
- **Exposed, consistent with existing behavior:** `a < y?.z > 3` and
  `a < y ?? z > 3` were swallowed as one token; now `?.`/`??` are counted as
  each reader already counts them without the `<` (C# counts both; JS counts
  `??`, not `?.`).
- **Known narrowing:** a nullable mark separated by whitespace
  (`List<int ?>`), or a multi-word type (`<unsigned int?>`), is no longer one
  token, so its `?` adds 1 to CCN. Legal but unidiomatic C#; not corrected.
- **Pathological input:** old pattern took 2.5 s on `<` + 20,000 `? ` and
  3.9 s on `<` + 20,000 `a?,`; new pattern about 0.01 s on both. Eight other
  adversarial shapes stay linear.
- **Tokenizer differential:** old and new patterns produce identical tokens
  on all 2,222 files of the repository.

## Proof

- PR head: full pytest 3151 passed / 7 skipped; `make` exit 0.
- PR merged onto trunk `d36ec0c5`: full pytest 3161 passed / 7 skipped;
  `make` exit 0. Trunk code was unchanged between that tree and the merge.

## Performance

Frozen 500-file corpus (300 repository Python sources, 200 generated
Java/C#/C++/TS/JS files dense in comparisons, generics and ternaries),
baseline trunk `c0d8eec4` vs PR head, limit 3% on the paired 95% upper bound.
Host was shared with other agent sessions throughout.

| Run | Load | Change | 95% CI | Verdict |
| --- | --- | --- | --- | --- |
| `perf/api.json` | 18–20 | −1.9% | −10.5% to +7.5% | bound not met |
| `perf/cli.json` (4 s) | 17–20 | +6.2% | +1.2% to +11.4% | bound not met |
| `perf/api-retry.json` (4 s) | 5–9 | +2.3% | −3.6% to +8.5% | bound not met |
| `perf/cli-retry.json` (4 s) | 7–8 | +1.7% | −2.2% to +5.8% | bound not met |

The retry was chosen before launch because the first runs were taken at load
18–20. `setup-error-*` files are an earlier attempt whose corpus still held
binary files. Tokenizer-only CPU-time microbenchmark (min of 41): +1.7% on the
whole corpus, −0.6% on the Python part, +0.2% to +3.8% on the generated parts.
Reading: no evidence of a material regression, but the strict bound is not
demonstrated on this loaded host.
