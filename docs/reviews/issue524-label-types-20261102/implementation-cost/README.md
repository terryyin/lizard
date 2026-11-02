# Runtime dispatch cost correction

Same active slice 1, G12. The bounded change removes class-only work from ordinary object members and groups rare syntax dispatch. All existing tests and 200 semantic expectations are unchanged. This is an uncommitted cost correction, not a performance qualification or independent refactor completion.

## Diagnosis and decision

The valid failed gate remains at `../evidence/performance1/current-calls-api-parameters.json`: 20 warmed alternating pairs, mean slowdown +4.7751675467%, paired 95% interval [3.5280258104%, 6.0373328721%]. Its strict upper bound is above 3%. No unfavorable observation, workload, or gate was removed or changed.

Root profiled those exact candidate bytes before correction. The retained current/candidate `profile1-*/profile.json`, `calls.prof`, and `top.txt` show the same 141,200 global-state calls and 43,200 member calls. Global-state own time was .109822 versus .096267 seconds; member own time was .023253 versus .015657 seconds. The candidate also made optional-context/member-role calls on ordinary call syntax. Profiling supplied diagnosis, not a timing verdict.

`decision.json` was recorded before editing or testing. It verifies all 108 pre-cost runtime hashes against root's `candidate1.json` and preserves the failed gate/profile references and hashes. The selected changes stay in three existing owners:

- `javascript_objects.py` checks the necessary class role before optional-name `endswith` and typed-member colon helpers. The existing helper still defines the complete semantic role.
- `typescript_states.py` groups the same declaration and marker token sets behind one gate. The declaration handler still precedes marker handling. Class-marker work remains on actual class questions. A trailing punctuation gate preserves backtick, colon and comma transitions before token-state updates.
- `typescript.py` groups question/colon preprocessing and checks the existing dialect flag before nested optional-member lookup. The optional token protocol, return annotation lookahead, tee/buffer resume, and yielded token ordering remain unchanged.

No ordinary-token wrapper, runtime bracket counter, stream buffer, workload switch, new public export, token-regex/core optimization, or semantic owner was added. Root's direct annotation imports, compatibility aliases, optional `is_optional` protocol and type-template `${` fix are preserved. ADR-0001 continues to govern the same public metric meanings.

## Preserved failed candidate and exact bytes

`before/lizard_languages/` retains the actual three pre-cost source files. `cost.patch` is the complete before/after patch; SHA256 `5a6a4281a793a5adf67e795ca4c81d3dcdde1ed3031d7660b4485745f9825b4b`. Together with the other 105 unchanged runtime files, these reconstruct the valid failed candidate, including root's direct-import and one-space corrections.

`manifest.json` contains all 108 before/after runtime hashes, the before/after test hashes, all ten current story product/test paths, line counts, and runtime provenance. Only these three runtime hashes changed:

| File | Before SHA256 | After SHA256 |
| --- | --- | --- |
| javascript_objects.py | 9b852cf0ed8933b3a385fc2d15cde552cce409c177fef18ecc472feb200aa94a | d9f484af96da6405d625429704a1d968f2949936b2cccf093e10b49f672b6c25 |
| typescript_states.py | 9ec29c64d7d9362eec004ffc2ffb77fb5d6e57849981973601e81422ff9781b8 | aba2b65df01d7c173c2649c9954f53b5e95a422947937947cd0dc1612fdc904a |
| typescript.py | 8a2b408b471810da54b2d999ce652866e6b7ec3489a61f8bed73cfdc9857d7f7 | c856cb2dbb693c4fa6c90a187d07aa9e7b0c9314add4798d5defa614ba528a05 |

All story product/test files remain at most 250 lines. All test hashes are unchanged.

## Current proof

Proof was selected before testing. Every command terminated with exit 0. The JSON receipts retain literal commands, resolved files, raw escaped output and matching source hashes.

| Receipt | Observed result |
| --- | --- |
| languages.json | 1,097 passed, 5 existing skipped; complete affected JS/TS/JSX/TSX/Vue, optional, template, signature and custom-dispatch selection. |
| consumers.json | 699 passed, 2 existing skipped; required binding/application/options/analyzer/parameter/extension/nesting/HTTP consumers and all regressions. |
| outputs.json | 64 passed; output, comment, website-smoke, CSV and output-file consumers. |
| fixture-capture.json | 200 passed and captured; `cases.json` exactly equals the accepted 200-case source/wrapper/public-record capture. |

The unchanged capture helper records actual wrapped `analyzed_source`, helper/filename provenance, observed public records and imported module paths/hashes. No parser logic or test assertion is replaced. The semantic red/green history remains in `../implementation/`; this correction does not relabel prior failures as passes.

The exact frozen 40-case corpus was also observed once before and once after through default and NS public analysis. `observe_calls.py` runs with `python -I`, inserts the intended checkout explicitly, and guards every loaded project module path. It uses the same absolute source filenames as root's retained profile protocol. It records outputs and dispatch call counts without durations or timing verdicts.

`calls-correspondence.json` proves unchanged corpus hashes and 80 equal observations, including file/function NLOC/tokens, names/long names/spans, full and filtered parameters/counts and NS nesting. All state/global/replay/next/end-statement counts are identical. Across this one default/NS pass:

| Operation | Before | After |
| --- | ---: | ---: |
| Object member dispatch | 864 | 864 |
| Member `str.endswith` | 864 | 0 |
| Class-marker helper | 80 | 0 |
| Typed-member helper | 280 | 60 |
| Optional-member context lookup | 80 | 60 |

These counts establish removed work on the actual failed workload. They do not establish the required slowdown bound.

## Consumers, ownership and remaining gates

`consumer-correspondence.json` reuses the root-accepted 719-consumer record at `../refactor-final/consumer-dispositions.json`. No broad consumer rediscovery was performed. The same three owners retain their imports, caller relationships, token protocol, observable schemas and metric meanings. `source-edges.json` explicitly dispositions six changed boundaries with their setups, current observing assertions and raw proof receipts: declaration/marker ordering, trailing punctuation, class-member roles, preprocessing/lookahead, API/parameter/NS records, and real CLI/HTTP/output/extensions. A consumer is not excluded because its own file was unchanged.

Prior non-JS/core/tokenizer/independent extension boundaries remain byte-identical and retain their accepted setup/observation correspondence. Current focused suites exercise all affected syntax and consumer purposes. Root owns fresh aggregate refactoring, full pytest/make and all 18 qualification gates on the same frozen inputs. Earlier label/annotation timing passes are not claimed as qualification of these new bytes. No pass prediction is made.

`git diff --check` terminated 0. No timing, full suite, make, formatter/hook lint, CI/observer action, commit, push, GitHub or workload mutation was performed by this implementer. No owned command remains running. Foreground Nix ownership was explicitly released after the last metadata command; only this documentation was completed afterward through `apply_patch`.
