# Work item identity

One contract for what identifies a queued or taken work item. Every backlog
operation, every reference to a story or correction, and every reconciliation
uses it. Do not restate a competing allocation rule elsewhere.

## The contract

1. **A work item has exactly one identity, allocated once.** Take it from the
   work item's canonical home: a story's seed ID with the story's anchor,
   including a new correction's story. A correction whose plan was already its
   home keeps the plan-path identity it recorded. Never give new work a
   plan-path identity to replace later, invent a number, keep a registry, or
   renumber. A story's plan records that story's identity and is never listed
   as work of its own.
2. **The identity is recorded, not derived.** Record it in the canonical home
   and write it in the backlog entry in full, beside the link. An entry that
   records nothing is still identified by the link it carries, until its
   identity is recorded.
3. **The link is navigation, not identity.** It says where the canonical home
   is now. Renaming the file, moving it to another directory, or changing the
   story anchor is a reference change: the recorded identity carries across
   unchanged, and the work item is still the same work item.
4. **A recorded value is never reallocated.** Repeating or resuming an
   operation reuses the identity that is already recorded. Two work items never
   share an identity, and one canonical home is never listed twice.
5. **Ambiguity stops for a human.** Conflicting recorded identities, a home
   that claims work another entry already claims, and an identity that cannot
   be read back from the entry it would be written into are refusals, not
   guesses. Report them and change nothing.

## In the backlog entry

    - [<title>](<canonical link>) — <identity> ([plan](<plan link>))

The identity and the plan link are each written only when the work item has
one. Omit the identity when the link already spells it exactly, which is the
ordinary case for a plan-homed correction still living at the path it was
identified by.

An older entry may carry only a seed ID where the identity now goes. It still
names the work item that seed ID plus the link's anchor named when it was
written; keep that work item's identity when you write the entry out in full.

## When the canonical home moves

The moved document's own recorded identity is what establishes that it is the
same work item. Record the identity in the moved document first, then update
the entry's link. Leave no second document recording the same identity.

## Allocate a new identity

Allocate only for new work. Take, resume, Preparing assignment, and moving a
canonical home keep the identity already recorded and allocate nothing. A new
seed number is above every seed number observed in the active seed directory,
in the done records and catalog beside the backlog, and in the published trunk
history of those directories. A new story anchor within an
existing seed yields a full identity none of those sources records. Removing a
seed file or story section never frees its identity.

Read that evidence with the
[identity-evidence operation](../SKILL.md#choose-a-new-identity) instead of
reading filenames. It reserves nothing; the canonical home records what you
choose.

- Run it from a checkout current with trunk: history covers only what that
  checkout reaches, at the `revision` the history source names.
- Number a new seed `next` or above, which exceeds the reported `highest`.
- Check the full candidate with `--identity <candidate>`. It is unused only
  when `candidate.status` is `unlisted` with no `history.record`, and, for a
  new seed number, no `history.seed`. Any other result means the identity was
  used: choose another.
- Treat every entry in `gaps` as unresolved before you publish a new home or
  claim; a report with gaps is not complete evidence. Rebuild an absent or
  mismatched catalog with `catalog-done` when done records exist, deepen a
  shallow clone, and read or ask about an unreadable record. A gap you cannot
  resolve stops the allocation for a person.
- Keep the project's naming, numeric padding, and anchor conventions.
  Renumbering a story whose identity is already published, or choosing
  between already published conflicting identities, stays with a person.

When `add` or startup admission refuses an identity because a done record
names it, allocate a different identity under this section, record it in the
draft, and leave the done record unchanged.
