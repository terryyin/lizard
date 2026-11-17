# Near-future direction check

Shared by [lizard-process-pr](../SKILL.md) and
[lizard-process-issue](../../lizard-process-issue/SKILL.md). The direction
itself lives only in the `## Near-future direction` section of
`.planning/PRODUCT-BACKLOG.md`; read it fresh each time and do not copy it here.

## When to check

Check once per PR or issue, after intake has established what it changes or
asks for and before any correction, repair, merge or other delivery work.
Review-only and reporting-only requests may proceed with their analysis, but
include the verdict in the result. A local change handed from
`lizard-process-issue` was already checked through its issue; recheck only if
the repair's scope grew beyond what was checked.

## Decide

Compare the actual content (the diff, or the expected behavior the issue asks
for), not its title or label, with the documented direction. Consider external
signatures (CLI options, output formats, public API, extension interfaces),
fundamental behavior, and how far the change extends current abilities.

- **Within the direction:** proceed automatically. Record a one-line verdict
  naming the direction clause it satisfies with the owner's evidence.
- **Beyond the direction, or unclear:** pause before further work and ask the
  developers for approval. Do not correct, repair, merge, comment on, or close
  the PR or issue while waiting. Backlog intake and the evidence already
  gathered may stay.

## Ask for approval

State in one message:

1. The PR or issue and what it changes or requests.
2. Why it goes beyond the near-future direction: quote the relevant clause and
   name the specific signature, behavior, or extension that crosses it.
3. A recommendation, **proceed** or **do not proceed**, with the reasons
   (value to users, cost, risk, maintenance burden, alternatives such as a
   narrower change that would fit).

Record the developers' decision on the owner. Approval covers only the scope
that was presented; material growth needs a new check. A decision not to
proceed follows the normal disposition (deferral, decline, or a backlog item for
later direction), with external actions only when authorized. When processing a
batch, pause only the affected members and continue the others.
