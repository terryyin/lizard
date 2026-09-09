# Disposable research

When a bounded, one-off investigation's raw trace will not guide later slices:

- Run it in a short-lived sub-agent or isolated context;
  prefer prompt-cache reuse when the host exposes it.
- Keep only the distilled conclusion in coordinator context.
- Paste that conclusion, not the raw tool dump, into the next delegation prompt.
- Default search and diagnostic evidence to
  filenames, match counts, bounded excerpts, or change statistics; do not load
  full generated files or unbounded logs into coordinator context.
