# Targeted retrieval and disposable research

When an applicable instruction or evidence passage is absent or truncated in
the current context:

- Retrieve the smallest complete passage from its authoritative location,
  using its heading, anchor, known line range, or a focused search. Include a
  linked prerequisite only when the passage depends on it.
- Retain the source location and the decision or conclusion the passage enables.
  Reuse that result while its source and applicable assumptions remain
  unchanged.
- Do not infer omitted text or recover it by rereading unrelated references or
  overlapping full-document batches. If the authoritative source or required
  passage remains unavailable, report the missing prerequisite so the affected
  work can stop.

When a bounded, one-off investigation's raw trace will not guide later slices:

- Run it in a short-lived sub-agent or isolated context;
  prefer prompt-cache reuse when the host exposes it.
- Keep only the distilled conclusion in coordinator context.
- Paste that conclusion, not the raw tool dump, into the next delegation prompt.
- Default search and diagnostic evidence to
  filenames, match counts, bounded excerpts, or change statistics; do not load
  full generated files or unbounded logs into coordinator context.
