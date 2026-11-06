# Source review — SEED-041#swift-computed-property-reporting

Launch: fresh general-purpose reviewer; source-only; no plan/product/proof.
Human amendment included: maintainer worth accepted ("worth it") after quieter-host
paired bounds all passed strictly below 3%.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Record a decision about Swift computed properties with an implicit getter and properties initialized by a closure: whether that reporting belongs in Lizard. | slice 1 | observed: quieter-host paired pass + maintainer worth accept; land PR #556 |
| 2 | Treat those properties as functions only when both of the following hold. | slice 1 | observed: both 2.1 and 2.2 satisfied before land |
| 2.1 | a maintainer accepts that the reporting is worth its cost | slice 1 | observed: developer reply "worth it" after bound pass |
| 2.2 | every required paired 95% upper slowdown is strictly below 3% | slice 1 | observed: /private/tmp/lizard-pr556-bench-quiet/{api,api-parameters,cli}-4s.json uppers 0.956% / 1.930% / 0.359% |
| 3 | Until both conditions in 2 hold, leave the Swift reader as it is and leave PR #556 unmerged. | slice 1 | observed: reader unchanged and PR open until both gates held |
| 4 | Change the Swift reader only after both conditions in 2 hold. | slice 1 | observed: land after quieter-host bound pass and worth accept |
| 5 | Keep function discovery in the existing Swift reader (`swift.py` / SwiftStates); do not add a second parser. | slice 1 | observed: PR #556 changes only lizard_languages/swift.py SwiftStates |
| 6 | On current trunk, before any reader change, confirm the key examples’ current-trunk behavior (see journey examples J1). | slice 1 | observed: trunk analyze_source_code — no score/handler; get/set/willSet present |
| 7 | Land a reader change only when its paired 95% upper slowdown bound is strictly below 3% on each of 7.1–7.3 vs current trunk. | slice 1 | observed: all three workloads passes under limit 3% |
| 7.1 | the affected Swift API | slice 1 | observed: /private/tmp/lizard-pr556-bench-quiet/api-4s.json upper 0.956% |
| 7.2 | public parameter names and counts | slice 1 | observed: /private/tmp/lizard-pr556-bench-quiet/api-parameters-4s.json upper 1.930% |
| 7.3 | broader CLI analysis | slice 1 | observed: /private/tmp/lizard-pr556-bench-quiet/cli-4s.json upper 0.359% |
| 8 | Do not land a result whose paired 95% upper bound is at 3% or above. | slice 1 | observed: all paired_95pct_ci uppers strictly below 3% |
| 9 | Require maintainer judgment that the worth of the reporting is greater than its cost; clearing the &lt;3% bound alone does not accept the change, and a worth judgment alone does not waive a failed bound. | slice 1 | observed: worth accepted only after bound pass; both required |
| 10 | Land a reader change only when both the worth judgment and the &lt;3% bound hold. | slice 1 | observed: both hold; merge PR #556 |
| 11 | If the pull request is still the vehicle when landing, preserve contributor history. | slice 1 | observed: merge commit 96b45e2ae parents aa783d0ed + 363cfb19ad; contributor 0f323dbfb ancestor of master |
| 12 | Otherwise (conditions not both met), record the judgment on the pull request and leave it unmerged. | slice 1 | observed: N/A — both conditions met; accept path taken |
| 13 | If the change is accepted, treat a later binding in the same declaration (e.g. `let a = 1, handler = { ... }`) as the same kind of property. | slice 1 | observed: candidate testSwiftAccessors.py / PR #556 behavior |
| 14 | Do not treat commas inside calls and generics as new functions. | slice 1 | observed: candidate testSwiftAccessors.py / PR #556 behavior |
| 15 | Keep today’s behavior for accessor blocks, stored properties, protocol requirements, and locals. | slice 1 | observed: candidate testSwiftAccessors.py 23 passed |
| 15.1 | accessor blocks | slice 1 | observed: candidate testSwiftAccessors.py accessor cases |
| 15.2 | stored properties | slice 1 | observed: candidate testSwiftAccessors.py stored controls |
| 15.3 | protocol requirements | slice 1 | observed: candidate testSwiftAccessors.py protocol controls |
| 15.4 | locals | slice 1 | observed: candidate testSwiftAccessors.py local controls |
| 16 | When both conditions hold and the change is accepted, analysis reports `score` and `handler` for the key-example properties, and those branches are not left only in the global scope. | slice 1 | observed: candidate analyze_source_code reports score and handler |

## Qualifications retained

Q1–Q5 as returned by the source-only reviewer (PR is candidate not acceptance; trunk baseline; revisit noisy measurements; human worth amendment; deferred other languages/PRs).

## Journey examples

J1–J4 as returned (trunk defect; accept reports score/handler; reject leaves trunk; multi-binding / comma rules).

## Exclusions

E1–E6 as returned (no merge-to-close-review; no abandoned candidate; no second reader; no land on ≥3%; no single-gate accept; deferred other languages/PRs).
