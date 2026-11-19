# Express browser intent through focused objects

Keep the scenario's order, concrete inputs, variants, and expected answers
visible in the test. Give shared browser mechanics one focused page or
component owner.

Find and reuse an existing owner before introducing one. Extend it with
operations named for the domain action or observation: `openItem(title)`,
`followRelatedItem()`, `showsItem(expectedTitle)`. Keep scoped locators, clicks,
keyboard interactions, and waits for observable UI state there.

Group the interactions for one user action behind one domain call.
For example, the step `When I add the note "Call back Friday"` calls
`notes.addNote("Call back Friday")`. The notes page object opens the dialog,
fills the field, and saves.

Name harness and builder helpers the same way when they express a scenario
step: the call states the domain action or precondition; encoding, wire-format,
locator, and byte mechanics stay inside the helper.

Inside an action, assert only the UI state it needs to proceed or finish. Make
the behavior under proof an explicit observation in the scenario: a scenario
about saving a note checks for the saved text, and one about dialog closure
uses a Save operation that leaves the closure check to it. The object can
implement the observation while the caller supplies the expected value. A
failed observation must retain expected and actual context.

Prefer fluent hand-offs. The scenario and its helpers get page objects from
one root, continue from each returned object, and construct none; keep
constructors out of their reach where the language allows. Opening a page
returns its object. Each later operation returns the object for what the user
now faces: the page reached, the dialog or panel left open, or `this` when
unchanged. Give a state its own type only when its actions or observations
differ. A step that cannot receive the previous object, such as a Gherkin step
definition, resumes through a root operation named for the assumed state:
`assumeItemPage("Atlas")` returns that page's object without navigating and
may assert what identifies it. For example:

```text
library = openLibrary()
item = library.openItem("Atlas", using = keyboard)
item.showsItem("Atlas")
related = item.followRelatedItem()
related.showsItem("Boreal")
rename = related.startRename()
rename.showsTitle("Boreal")
related = rename.cancel()
```

For the mouse variant, pass `using = mouse` at the same call site; keep the
choice explicit when the variant is under proof. Use the project's
asynchronous and queue mechanics; awaited calls on the returned objects form
such a chain. Do not introduce Cypress, a base class, phase/cue runner,
Gherkin, or framework conversion to achieve a chain.

Wait for a meaningful observation such as a visible selected item, an enabled
control, or a completed handoff. Keep transport synchronization that establishes
causality at its independent boundary. For example:

```text
heldRead = holdItemRead("atlas")
item = library.openItem("Atlas")
heldRead.received()
item.showsLoading()
assert capturedRequests == ["atlas"]
heldRead.release()
item.showsItem("Atlas")
```

The held response, request count, captured records, and independent measurements
remain inspectable outside UI mechanics when they prove the behavior; a page
object's declaration that it happened is no substitute. Preserve exact bytes or
contracts and caller expectations under proof. Direct API setup or observation
remains appropriate at its meaningful boundary.

Avoid methods that bury a whole scenario, select their own expected answers, or
conceal causally important ordering. Improve implicated tests within the
authorized work; leave unrelated suites alone.
