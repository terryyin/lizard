# Release Lizard 1.25.0

<a id="release125"></a>

### Publish the next minor release

**Identity:** SEED-027#release125
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"652fffb271a9e8e02dd57fea73192f070a2b7f4b272bd65afa66627794774eb3"}}
```

**Goal:** Lizard users can install the next minor release, 1.25.0, from PyPI
and find its version tag and release notes on GitHub.

**Source:** The human requests: "make a new release bump up the minor version."

**Scope:** One bounded release slice under this contextual instruction:
update CHANGELOG.md for changes since 1.24.3, generate lizard_ext/version.py
through setup.py, independently review/refactor the change, validate the full
suite and make, build/check/install the distributions and verify 1.25.0 and
representative packaged analyzers. Publish the qualified change to master,
push the numeric 1.25.0 tag to trigger the existing release.yml PyPI workflow,
publish the matching GitHub release, and verify successful publication and
the actual PyPI distribution. Finish ordinary recoverable lifecycle cleanup.
No new parser implementation, website deployment, dependency change or release
workflow redesign is requested. Preserve existing files and earlier versions.

**Known expectations:** Both current package metadata and the latest GitHub
and PyPI release identify 1.24.3; 1.25.0 is the next minor version and its tag
does not exist. CHANGELOG.md owns the version; setup.py generates version.py.
Existing numeric tag pushes invoke build and trusted PyPI publish jobs.
Significant merged work includes Dart and fixed-format COBOL readers, Python
nesting and JS/TS/Vue parsing corrections. Notes must describe verified scope
without claiming all remaining #519 behavior is fixed.

**Authority:** Implement and publish this release, including commits, master
integration, version tag, GitHub release and the repository's existing PyPI
publication workflow. Run the required checks before publishing artifacts.
Do not replace or force-update a published tag or package version. If any
required verification/publication fails, retain its exact state and repair
only the authorized release work or report the missing external prerequisite.
