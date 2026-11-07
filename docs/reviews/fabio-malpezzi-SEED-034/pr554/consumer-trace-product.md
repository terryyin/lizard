# Product consumer trace — PR #554

## Changed production paths

- `lizard_ext/lizardns.py` — after `.` or `?.`, do not treat the next token as
  a structure head when it matches a structure keyword.

## Consumers

- Real `try`/`catch` and `if` remain structures.
- Promise `.then` / `.catch` / `?.catch` member names are not structures.
- Other languages' NS use is through the same extension; member-dot pattern is
  JS-style but the state is generic for `.` / `?.` tokens.
