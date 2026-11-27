"""
PL/SQL keyword preprocessing: the token-level keyword handling applied before
condition counting.

- PL/SQL keywords are case-insensitive, so whole condition keyword tokens
  match their lowercase conditions in any casing. Identifiers that merely contain a
  keyword (If_And_Ready, p_And) keep their source spelling.
- "END IF", "END LOOP", "END CASE", "END WHILE" and "END FOR" are merged into
  single tokens so their trailing keyword is not counted as a condition.
- EXIT WHEN: The WHEN keyword is filtered out because "EXIT WHEN condition" is
  not a branching construct - it's a conditional exit that doesn't create
  alternate execution paths. The LOOP itself adds complexity.
"""

_END_COMPOUND_KEYWORDS = ("IF", "LOOP", "CASE", "WHILE", "FOR")


class _Keyword(str):
    """A keyword token keeping its source spelling but matching any casing."""

    def __eq__(self, other):
        return isinstance(other, str) and self.lower() == other.lower()

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.lower())


def merge_compound_keywords(tokens, keywords):
    """
    Mark whole tokens spelling one of the uppercase keywords in any casing,
    so condition counting and extensions that remove condition categories
    match them while declaration and long-name text keep the source
    spelling. Merge compound keywords to prevent the condition counter from
    double-counting:
    - "END IF", "END LOOP", "END CASE", "END WHILE", "END FOR" -> single tokens
    - "EXIT WHEN" -> remove the WHEN keyword (EXIT doesn't create a branch)
    """
    last_nonwhitespace_token = None
    last_upper = None
    pending_tokens = []

    for token in tokens:
        if not token.isspace() or token == "\n":
            token_upper = token.upper()

            # Handle "END IF", "END LOOP", etc.
            if last_upper == "END":
                if token_upper in _END_COMPOUND_KEYWORDS:
                    # Merge into "END_IF", "END_LOOP", etc.
                    yield "END_" + token_upper
                    last_nonwhitespace_token = None
                    last_upper = None
                    pending_tokens = []
                    continue

            # Handle "EXIT WHEN" - skip the WHEN keyword
            if last_upper == "EXIT" and token_upper == "WHEN":
                # Skip this WHEN keyword
                pending_tokens = []
                continue

            # Yield any pending tokens
            if last_nonwhitespace_token:
                yield last_nonwhitespace_token
            for pending in pending_tokens:
                yield pending
            pending_tokens = []

            # Update tracking
            last_nonwhitespace_token = (
                _Keyword(token) if token_upper in keywords else token)
            last_upper = token_upper
        else:
            # Accumulate whitespace
            pending_tokens.append(token)

    # Don't forget the last tokens
    if last_nonwhitespace_token:
        yield last_nonwhitespace_token
    for pending in pending_tokens:
        yield pending
