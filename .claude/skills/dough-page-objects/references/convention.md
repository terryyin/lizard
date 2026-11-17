# Express browser intent through focused objects

Apply this convention only after the test entry selects it. Keep the scenario's
order, concrete inputs, variants, and expected answers visible in the test. Give
shared browser mechanics one focused page or component owner.

Find and reuse an existing owner before introducing one. Extend it with
operations named for the domain action or observation: `openItem(title)`,
`followRelatedItem()`, `showsItem(expectedTitle)`. Keep scoped locators, clicks,
keyboard interactions, and waits for observable UI state there.

Express each meaningful user action as one Gherkin step or page-object call.
Group the UI interactions that carry out the action inside the object. For
example, `When I add the note "Call back Friday"` can call
`notes.addNote("Call back Friday")`. The operation opens the dialog, clears and
fills the field, clicks Save, and asserts that the dialog closes.

Name harness and builder helpers the same way when they express a scenario
step: the call states the domain action or precondition; encoding, wire-format,
locator, and byte mechanics stay inside the helper.

Inside an action, assert only the specific UI state it needs to proceed or finish.
Make the behavior under proof an explicit observation in the scenario. A
scenario about saving a note explicitly checks for the saved text. For a
scenario about dialog closure, use a Save operation that leaves the closure
check to the scenario. The object can implement an explicit observation while
the caller supplies the expected value where applicable. A failed observation
must retain meaningful expected and actual context.

For example, in language-neutral pseudocode:

```text
library = openLibrary()
item = library.openItem("Atlas", using = keyboard)
item.showsItem("Atlas")
related = item.followRelatedItem()
related.showsItem("Boreal")
```

For the mouse variant, pass `using = mouse` at the same call site. Keep this
keyboard/mouse choice explicit when the variant is under proof. Fluent
methods may return the object reached next, or `this` when remaining in the same
place. Use the project's construction, asynchronous, and queue mechanics. Do not
introduce Cypress, a base class, phase/cue runner, Gherkin, or framework conversion just to
achieve a chain. A short sequence of awaited operations is equally valid.

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
remain inspectable outside UI mechanics when they prove the behavior. Do not
replace such evidence with a page object's declaration that it happened. Preserve
exact bytes or contracts and caller expectations under proof. Direct API setup or
observation is appropriate at its meaningful boundary; this convention is not a
ban on it.

Keep objects focused on the page or component they own, with domain-named actions
and observations. Avoid methods that bury a whole scenario, select their own
expected answers, or conceal causally important ordering. Improve implicated
tests within the authorized work; leave unrelated suites alone.
