## Maintainer judgment (SEED-042 / slice 1)

Thank you for the clear inline-suite examples and the careful reader patch in this PR.

We evaluated head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` plus the required shared-line docstring NLOC correction (`def value(): """A value."""; return 1` → NLOC 1), and one cheaper alternative in the same Python reader owners that keeps the `PythonIndents` pending-function finish / same-line NLOC fix but avoids the per-token bracket-depth / continuation work in `PythonReader.preprocess`.

**Behavior:** both candidates restore the story examples (`Stream.read` / `Stream.close`, nested `outer` kept beside `outer.inner`, shared-line docstring NLOC 1) while leaving block `kept` at CCN 2 / NLOC 4.

**Performance gate (required):** paired 95% upper slowdown must be **strictly below 3%** on frozen Python corpus workloads `api`, `api-parameters`, and `cli` versus current trunk. Measured on this host against trunk `b1052466a472010d753c0853966225514603a069` with a frozen 295-file Python corpus:

| Candidate | Workload | Paired 95% CI (% slowdown) | Gate |
| --- | --- | --- | --- |
| PR head + NLOC fix | api | +11.83 … +13.60 | fail |
| PR head + NLOC fix | api-parameters | +14.29 … +15.55 | fail |
| PR head + NLOC fix | cli | +9.71 … +12.24 | fail |
| Structure-only cheaper alt | api | +1.92 … +3.31 | fail (upper ≥ 3) |
| Structure-only cheaper alt | api-parameters | +1.94 … +3.29 | fail (upper ≥ 3) |
| Structure-only cheaper alt | cli | +1.05 … +2.37 | pass |

Neither candidate clears **all** required gates. Per SEED-042, trunk’s Python reader stays unchanged and this PR stays **unmerged**. The behavior is desirable; the cost on the API / parameter-name workloads is what blocks landing.

We are not asking for further force-pushes on `ahuimanu:fix/python-inline-suites`. If a future approach can keep these examples with every required paired upper bound strictly below 3%, we can reopen the disposition then.
