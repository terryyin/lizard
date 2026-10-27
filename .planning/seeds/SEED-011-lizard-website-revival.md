---
id: SEED-011
status: active
planted: 2026-10-27
planted_during: Owner request to revive lizard.ws
trigger_when: Restore the public Lizard homepage and online demo
scope: bounded
---

# SEED-011: Lizard website revival

<a id="revive-lizard-website"></a>

### Revive the Lizard homepage and online demo

**Identity:** SEED-011#revive-lizard-website
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/004-lizard-website-revival/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"d62811fef2c672ee13530b7a982611bcbace01bfd060c5f51a982a4507582e83","plan":"cb0f33384727cf8e02bbe19a3aae3cba45b526d673f689a0e775a29494ed9a35"}}
```

**Goal:** Developers visiting lizard.ws can understand Lizard, install it,
find its documentation, and try source-code analysis in a usable online demo.

**Scope:** Restore the existing GCP-hosted site with a supported Python 3
web runtime and refreshed accessible, mobile-friendly homepage. Reuse Lizard's
analysis API. The demo accepts pasted code with a language or filename,
offers useful local examples, and shows function metrics with concise
explanations. Bound requests and analysis time, report input and service
errors clearly, and do not persist or log submitted source code. Preserve
the existing `/analyze` JSON route and public logo asset URLs. Publish a
verified preview before switching production traffic; establish HTTPS and
a canonical domain where existing access permits. Document deployment,
verification, and recovery. Keep parser behavior unchanged.

**Authority:** The owner selected “Project homepage + online demo”, accepted
the proposed Flask/App Engine migration and release sequence, signed in to
gcloud, and authorized the remaining work. Use gcloud directly; other
repository tooling uses `nix develop -c`.

**Key examples:**

1. A visitor opens the homepage on a phone, sees what Lizard does, copies
   installation and usage commands, and follows working documentation,
   GitHub, and PyPI links.
2. A visitor loads a source example or pastes code, selects its language,
   runs analysis, and sees the returned function names, NLOC, CCN, token
   count, and parameter count with a clear completion status.
3. A malformed or oversized request receives a clear client error; service
   failure restores the interface to a usable state without exposing the
   submitted source or internal exception detail.
4. Existing `/analyze` callers still receive JSON analysis results, and
   published logo URLs continue to resolve.
5. A new App Engine version passes preview checks before traffic moves;
   public HTTP redirects to the chosen HTTPS domain, whose page and demo
   work. Recovery instructions identify how to return traffic to the prior
   version.

**Deferred promises:** Accounts, stored reports, repository cloning,
whole-project uploads, billing dashboards, parser changes, a larger hosted
analysis service, and a move to another hosting platform.

**External condition:** GCP project `lizardanalyzer` is active, but its sole
linked billing account was observed closed with `billingEnabled=false`.
The owner has been asked to reopen it. Local implementation and proof can
continue; credentialed deployment and production verification wait for
billing and service access. Domain registrar changes may require owner
action if the current GCP account cannot control apex DNS.

**Plan:** [Executable plan](../slice-plans/004-lizard-website-revival/PLAN.md)
