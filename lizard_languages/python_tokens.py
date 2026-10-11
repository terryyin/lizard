"""Python tokens and f-string interpolation expressions."""

from .code_reader import CodeReader


# Triple-quoted string patterns for the common tokenizer, so a whole
# triple-quoted string is matched as a single token.
_PY_TRIPLE_QUOTE = (
    r"|(?:\"\"\"(?:\\.|[^\"]|\"(?!\"\")|\"\"(?!\"))*\"\"\")"
    r"|(?:\'\'\'(?:\\.|[^\']|\'(?!\'\')|\'\'(?!\'))*\'\'\')"
)


# str/bytes f-string prefixes (any case). Excludes bare b/rb (not f-strings).
_FSTRING_PREFIXES = frozenset(('f', 'rf', 'fr', 'bf', 'fb'))


def generate_tokens(source_code, addition='', token_class=None):
    # Python comments end at the physical newline; a trailing '\' does not
    # extend the comment to the next line (issue #317).
    # '//' is the floor division operator, not the start of a comment.
    tokens = CodeReader.generate_tokens(
        source_code,
        r"|\#[^\n]*|//=?" + _PY_TRIPLE_QUOTE + addition,
        token_class)
    return _expand_fstring_interpolations(tokens, token_class)


def _expand_fstring_interpolations(tokens, token_class):
    """Re-tokenize the {...} interpolations of f-strings so control-flow
    keywords inside them reach the condition counter.

    Addresses GitHub issue #317: control flow inside interpolations was
    invisible to the tokenizer when the f-string was one opaque string token.

    The tokenizer emits the f-string prefix ('f', 'rf', ...) as its own
    token right before the string body, so an f-string is a prefix token
    immediately followed by a string token.
    """
    prefix = None
    for token in tokens:
        if prefix is not None:
            held, prefix = prefix, None
            if isinstance(token, str) and token[:1] in ('"', "'"):
                yield held
                for inner in _tokenize_fstring_body(
                        token, token_class):
                    yield inner
                continue
            yield held
            # not a string after all; fall through to classify this token
        if isinstance(token, str) and \
                token.lower() in _FSTRING_PREFIXES:
            prefix = token
            continue
        yield token
    if prefix is not None:
        yield prefix


def _skip_quoted_literal(body, pos):
    """Advance past a Python string literal in body; return new position."""
    n = len(body)
    if pos + 2 < n and body[pos:pos + 3] in ('"""', "'''"):
        quote = body[pos:pos + 3]
        pos += 3
        while pos < n:
            if body[pos:pos + 3] == quote:
                return pos + 3
            if body[pos] == '\\':
                pos += 2
                continue
            pos += 1
        return n
    if pos < n and body[pos] in '"\'':
        quote = body[pos]
        pos += 1
        while pos < n:
            if body[pos] == '\\':
                pos += 2
                continue
            if body[pos] == quote:
                return pos + 1
            pos += 1
        return n
    return pos


def _find_interpolation_end(body, start):
    """Return index after the closing '}' of an interpolation opened at start."""
    depth = 1
    j = start + 1
    n = len(body)
    while j < n and depth:
        if body[j] in '"\'':
            j = _skip_quoted_literal(body, j)
            continue
        if body[j] == '{':
            depth += 1
        elif body[j] == '}':
            depth -= 1
        j += 1
    return j


def _tokenize_fstring_body(token, token_class):
    """Split an f-string body into literal chunks (kept as string tokens)
    and interpolation expressions (re-tokenized so inner keywords count).

    '{{' and '}}' are literal braces, not interpolations. Brace matching
    inside an interpolation skips string literals so a '}' nested in quotes
    does not end the expression early. If the body has no interpolation,
    the original token is yielded unchanged.
    """
    quote = token[:3] if token[:3] in ('"""', "'''") else token[:1]
    body = token[len(quote):len(token) - len(quote)]
    literal = []
    produced = []
    i, n = 0, len(body)
    while i < n:
        if body[i:i + 2] in ('{{', '}}'):
            literal.append(body[i])
            i += 2
            continue
        if body[i] == '{':
            j = _find_interpolation_end(body, i)
            if literal:
                produced.append(quote + ''.join(literal) + quote)
                literal = []
            # The interpolation is Python code: tokenize it as such, which
            # also expands any nested f-string (recursion terminates on the
            # ever-shorter interpolation body).
            produced.extend(generate_tokens(
                body[i + 1:j - 1], '', token_class))
            i = j
            continue
        literal.append(body[i])
        i += 1
    if not produced:
        yield token  # no interpolation; leave the string token untouched
        return
    if literal:
        produced.append(quote + ''.join(literal) + quote)
    for tok in produced:
        yield tok
