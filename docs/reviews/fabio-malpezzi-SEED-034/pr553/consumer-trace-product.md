# Product consumer trace — PR #553

## Changed production paths

- `lizard_languages/typescript.py` — `OPTIONAL_NAME` regex includes quoted
  string names before `?` when followed by `:` / `,` / `)` / end.

## Shared-language consumers

- JS tokenizer does not use this TypeScript-only pattern; no JS change.
- CCN still counts real `?` ternary tokens; regression `g(c){return c?'a':'b'}`
  remains CCN 2.
- Optional chaining `?.` and nullish `??` remain separate tokens.

## Decision

Merge as-authored; no consumer correction.
