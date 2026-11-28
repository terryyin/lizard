# Shared MakeMe convention

## Use the builder

Use the project's shared test-data factory, domain-named chained operations,
and the project's completion operation. To continue from a value the test
already holds, such as fixture output or an earlier result, start from the
factory's `the<Shape>(value)` access rather than a helper that takes the value.
When the builder lacks what your test needs or makes setup awkward, improve the
shared builder as part of writing the test.

```text
reader = makeMe.aReader.identifiedAs("reader-7").please()
note = makeMe.aNote.ownedBy(reader).please()

actual = access.canRead(note, reader)
expect(actual).toEqual(true)
```

State meaningful identities, times, and other scenario facts at the call site
through builder operations. Leave incidental values to the builder's defaults.
The note's owner matters here, and the builder can supply its title.

Reach a stereotype example that tests share through its named example on the
builder (a data mother). The name stands for the facts the scenario relies on;
state at the call site only what the test varies or checks.

```text
catalog = makeMe.aCatalog.thatHoldsTheClassics.withoutTitle("Emma").please()
```

The examples are pseudocode. Follow the project's language and established
construction boundary. Completion may return data or use a project-owned
persistence harness.

Keep actions, independent expected answers, and captured evidence outside
builders. State exact bytes, legacy shapes, and deliberate invalidity under
proof in the test.

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

## Extend the builder

Reuse the builder that owns the canonical domain shape and its relationships.
Add one to the shared factory if that shape has no builder yet.
Each factory access supplies a fresh builder, and each completed value owns its
mutable state.

Add a builder operation, default, `the<Shape>` access or named example only to
meet a concrete setup need in a consuming test or to produce valid data. Rely
on the consuming test to exercise the setup it depends on and on existing
validation to check the data. Skip tests dedicated to the builder itself.
Remove builder behavior when neither reason applies.

Add a named example when tests repeat the same stereotype data. Build it from
the builder's own operations and return the builder, so a test can refine it
before completion; a name that ends in the completion operation may complete
it instead. Let tests read the facts they must name, such as identities, from
the example rather than repeat them.

Use sensible, preferably deterministic defaults for fields needed only for
validity, including incidental datetimes. Use a current default when validity
requires freshness. Fill only missing data and derive related defaults
consistently. Preserve explicit values, including deliberate invalidity. For
defaults that depend on other choices, prefer filling them just before completion
with `beforePlease` or the project's equivalent. Overridable defaults set during
construction also work when they preserve those choices. Use the builder's
existing completion mechanism.

When simplifying or speeding up setup, preserve scenario facts, independent
observations, and isolation. Share immutable preparation or safely resettable
resources only with demonstrated isolation. Keep the observations and measured
saving that justify an optimization.
