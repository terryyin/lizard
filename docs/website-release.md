# Releasing lizard.ws

See [local development](website.md), [Actions identity](website-deployment-identity.md)
and [manual operations/current public version](website-operations.md).

## Publish an immutable website tag

The `Website release` workflow validates and builds the tagged source, deploys a
non-serving preview, verifies it, promotes it and verifies the public website.
Publish the chosen commit with Git directly:

```sh
git tag -a web-1.2.3 -m 'Website 1.2.3' COMMIT
git push origin refs/tags/web-1.2.3
```

Lightweight tags also work. Components are nonnegative decimal integers with no
leading zeroes except `0`; `web-0.0.0` is valid. Dots map to hyphens, so
`web-1.2.3` deploys as `web-1-2-3`. The mapped name must be at most 32 characters
for this project/service. Prerelease/build suffixes are unsupported. There is no
numeric ordering policy: the maintainer chooses the release. Never move a
published tag or reuse it for different source.

Only `web-*` tag pushes select `.github/workflows/website-release.yml`. Malformed
website tags fail the credential-free validation job; authentication/deployment
cannot start. Branches, pull requests and package tags do not select website
deployment. The PyPI workflow remains separate. Tag text is environment data,
never interpolated shell code.

Validation peels the immutable event object and compares it with the checkout.
The release job checks out that validated commit, including annotated tags.
Reruns use the original object, never today's branch head or a moved tag. Recover
an unavailable original object before retrying; the workflow does not guess.

## Validation, preview and public verification

Every valid tag runs Python tests/checks, the declared Node/npm, `npm ci`, build
and all browser scenarios regardless of branch path filtering. Authentication
follows those checks. Deployment uploads that tested generated bundle without
rebuilding, with `--no-promote --no-stop-previous-version`.

Before deployment, the run captures the complete service traffic split and the
candidate's existing source metadata. A preview must pass normal TLS, homepage,
all generated assets, original logos/favicon and real analysis (one function,
CCN 3, NLOC 6, one parameter). App Engine deployment can finish before initial
dispatch is ready: homepage 5xx gets a finite two-minute startup wait, with at
most ten seconds per request and five-second spacing. Persistent failure fails;
content, assets and metrics assertions are not retried into success.

Only a successfully verified preview can reach promotion. The workflow then
checks actual target traffic and source metadata as well as public HTTPS,
assets, real analysis and the HTTP-to-HTTPS redirect with path/query preserved.
A healthy old homepage alone is not acceptance of the new release.

## Inspect a run and recover

The final summary and `website-source-attempt-N` / `website-release-attempt-N` artifacts retain tag,
event object, peeled/checked-out source, target/version, upload list, both URLs,
prior and observed current traffic, stage/result and exact recovery command.
Source/tag also remain in the App Engine version's environment metadata.
Browser reports contain test fixtures; generated credentials are excluded.
Each run attempt keeps separate evidence. A release-job-only rerun consumes the
source artifact name exported by its successful validation job, preserving the
original validation source and earlier attempt results.

| Failure stage | Traffic and next action |
| --- | --- |
| Validation, tooling/checks or authentication | No traffic mutation is attempted; phase/result remain visible even when no split could be captured |
| Inspection, deployment or preview | Promotion is blocked; inspect the error and retained source/traffic evidence |
| Promotion or public verification | The run fails and records current traffic or explicitly says unknown; inspect before restoring the saved split |

Wait until the release workflow stops before manual traffic restoration. Use the
run's exact `gcloud app services set-traffic` command, which retains every prior
allocation and explicit `--split-by` (`cookie`, `ip` or `random`). When the
service omits sharding, recovery uses gcloud's IP default. Restore traffic rather
than moving tags, rebuilding or deleting versions. No automatic rollback or
version cleanup occurs. Keep both old and failed/new versions for diagnosis.

Retry a failed run for the same immutable source with Actions **Re-run jobs**.
Redeployment of an existing version requires established matching source and
zero public traffic. A matching already-serving version is verified without
redeploying; an already-full traffic assignment is also left unchanged.
Unknown/different source or traffic state fails with inspection/recovery guidance;
never overwrite a serving mismatch. Fix source defects with a new immutable tag.

One `website-release` concurrency group serializes release mutations with
`cancel-in-progress: false`: running deployments are not cancelled. GitHub keeps
only one pending run per group; a newer pending run can replace an older one.
Inspect cancelled pending runs and rerun the intended tag when the group is
free. This is not semantic-version ordering. Manual rollback must not race a run.
See [GitHub concurrency](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-workflow-concurrency)
and [gcloud traffic splitting](https://docs.cloud.google.com/sdk/gcloud/reference/app/services/set-traffic).

## Preview rollout evidence

Verified on 2026-10-27; all tags remain immutable:

| Tag | Observed result |
| --- | --- |
| `web-01.0.0` | [Validation failed](https://github.com/terryyin/lizard/actions/runs/40486098691); preview job, authentication and deployment skipped |
| `web-0.0.0` | [First preview failed](https://github.com/terryyin/lizard/actions/runs/40486246278) when App Engine aborted initial dispatch before assigning an instance; traffic stayed unchanged; bounded readiness was added |
| `web-0.0.2` (lightweight) | [Verified preview](https://github.com/terryyin/lizard/actions/runs/40488146510), version `web-0-0-2` |
| `web-0.0.3` (annotated) | [Verified preview](https://github.com/terryyin/lizard/actions/runs/40488849369), version `web-0-0-3` |

Both successful runs deployed tested source
`5d320dd497c0eb96d3f57cf5521ec05a8bfdb510`, runtime `python313`, with matching
source/tag metadata, normal HTTPS, every generated asset and real sample metrics
(CCN 3, NLOC 6, one parameter). The annotated event object
`6daa7515aeb9930b5b826ff7e57a454835c1e7f1` peeled to that checked-out source.
Complete before/after splits and subsequent readback all retained
`revive-20261027-f818daf: 1.0`; neither run promoted public traffic.
