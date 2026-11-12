# Shared MakeMe convention

Use the project's one shared test-data factory. Each factory access supplies a
fresh builder and each completed value owns its mutable state. Reuse or extend
the builder that owns the canonical domain shape and its relationships.

Use domain-named chained operations and the project's completion operation:

```text
reader = makeMe.aReader.identifiedAs("reader-7").please()
note = makeMe.aNote.ownedBy(reader).please()
tracker = makeMe.aTracker.forNote(note).please()

actual = access.canRead(note, reader)
expect(actual).toEqual(true)
```

The note's owner matters to this case; its incidental title can use a sensible,
preferably deterministic default. Related builders derive consistent ownership
and identities. State meaningful identities, time and other scenario facts at
the call site. Prefer builder operations to later mutation and domain operations
to repeating field lists. Another test receives independent mutable values.

This is pseudocode. Factory access, imports, typing, completion and persistence
follow the project's language and established construction boundary. Completion
may return data or use a project-owned persistence harness; no Java persistence
default or JavaScript import path is universal.

Keep actions, independent expected answers and captured evidence outside builders.
Exact bytes, legacy shapes and deliberate invalidity under proof stay visible:

```text
account = makeMe.anAccount.ownedBy(reader).please()
malformedWire = "{broken"
actual = parser.read(malformedWire, account)
expect(actual.error).toEqual("invalid document")
```

Build the surrounding valid state through the shared API without constructing
the expected answer there. Preserve dependent relationships needed to observe
destructive behavior. Resource lifecycle, environment setup and controlled clocks
remain with their project owners.

For concise or faster setup, extend the existing construction API and improve
its implementation or harness. Preserve scenario facts, independent observations
and isolation. Share immutable preparation or safely resettable resources only
with demonstrated isolation; keep the surviving observations and measured saving
that justify an optimization. Do not bypass the builder or share mutable test
data as a shortcut.
