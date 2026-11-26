"""Python tokens and f-string interpolation expressions."""

from .token_pattern import compiled_token_pattern, tokens_in_span


class FStringExpressionToken(str):
    '''An f-string token whose delimiters belong to the interpolation.'''


class ContinuedLineToken(str):
    """A physical newline inside a grouping delimiter."""

    is_continuation = True


class PrefixedStringToken(str):
    '''A string written directly after its prefix token (f, r, b, ...).'''


class FStringLiteralToken(str):
    '''A literal chunk of an f-string split by its interpolations: part of
    the f-string expression, never a standalone string.'''


# Triple-quoted string patterns for the common tokenizer, so a whole
# triple-quoted string is matched as a single token.
_PY_TRIPLE_QUOTE = (
    r"|(?:\"\"\"(?:\\.|[^\"]|\"(?!\"\")|\"\"(?!\"))*\"\"\")"
    r"|(?:\'\'\'(?:\\.|[^\']|\'(?!\'\')|\'\'(?!\'))*\'\'\')"
)


# str/bytes f-string prefixes (any case). Excludes bare b/rb (not f-strings).
_FSTRING_PREFIXES = frozenset(('f', 'rf', 'fr', 'bf', 'fb'))
# All string prefixes (any case), which the tokenizer emits apart from the string.
_STRING_PREFIXES = _FSTRING_PREFIXES | frozenset((
    'r', 'u', 'b', 't', 'br', 'rb', 'tr', 'rt'))


# Match at the start of a word token, before the common pattern reads \w+.
_INTERPOLATED_START = r"""|(?P<interpolated>(?i:fr|rf|bf|fb|f|tr|rt|t))(?=['"])"""
_GROUPING = r"|(?P<grouping>[()[\]{}])"
_CRLF_JOIN = r"|(?P<crlf_join>\\\r\n)"
_PY_ADDITION = r"|\#[^\n]*|//=?" + _PY_TRIPLE_QUOTE
_CLOSERS = {'(': ')', '[': ']', '{': '}'}


def generate_tokens(source_code, addition='', token_class=None, *, interpolate=True):
    if not interpolate:
        pattern = compiled_token_pattern(_PY_ADDITION + addition)
        return _mark_prefixed_strings(
            tokens_in_span(pattern, source_code, 0, len(source_code), token_class))
    pattern = compiled_token_pattern(
        _PY_ADDITION + _INTERPOLATED_START + _GROUPING + _CRLF_JOIN + addition)
    return _mark_prefixed_strings(_source_tokens(source_code, pattern, token_class))


def _source_tokens(source, pattern, token_class):
    start = depth = 0
    while True:
        for match in pattern.finditer(source, start):
            token = match.group()
            value = token_class(match) if token_class else token
            kind = match.lastgroup
            if kind == 'grouping':
                depth = max(0, depth + (1 if token in _CLOSERS else -1))
            elif kind == 'interpolated' and isinstance(value, str):
                string = _read_string(source, match.end(), token.lower(), (pattern, token_class))
                if string is not None:
                    start, parts = string
                    yield value
                    yield from parts
                    break
            elif kind == 'crlf_join' and token_class is None:
                value = '\\\n'
            if depth and token in ('def', 'class'):
                # A declaration cannot belong to an unclosed expression.
                depth = 0
            if token_class is None and token == '\n' and depth:
                yield ContinuedLineToken(token)
            else:
                yield value
        else:
            return


def _mark_prefixed_strings(tokens):
    """Keep the prefix and the string as separate tokens."""
    prefix = None
    for token in tokens:
        if prefix is not None:
            yield prefix
            prefix = None
            if isinstance(token, str) and token[:1] in ('"', "'") and not isinstance(
                    token, (FStringExpressionToken, FStringLiteralToken, PrefixedStringToken)):
                yield PrefixedStringToken(token)
                continue
        if (isinstance(token, str) and token.lower() in _STRING_PREFIXES and
                not isinstance(token, FStringExpressionToken)):
            prefix = token
        else:
            yield token
    if prefix is not None:
        yield prefix


def _read_string(source, start, prefix, scanner):
    quote = source[start:start + 3]
    if quote not in ('"""', "'" * 3):
        quote = quote[0]
    pos = literal_start = start + len(quote)
    parts = []
    raw = 'r' in prefix
    if 't' in prefix:
        scanner = (scanner[0], None)
    while pos < len(source):
        if source.startswith(quote, pos):
            end = pos + len(quote)
            if not parts or 't' in prefix:
                return end, [PrefixedStringToken(source[start:end])]
            _append_literal(parts, source, literal_start, pos, quote)
            if literal_start == pos and _ends_on_empty_line(parts):
                parts.append(FStringLiteralToken(quote + quote))
            return end, parts
        char = source[pos]
        if source[pos:pos + 2] in ('{{', '}}'):
            pos += 2
        elif char == '{':
            field = _read_field(source, pos + 1, quote, raw, scanner)
            if field is None:
                return None
            _append_literal(parts, source, literal_start, pos, quote)
            pos, tokens = field
            parts.extend(FStringExpressionToken(token) for token in tokens)
            literal_start = pos
        elif char == '\\':
            pos = _after_escape(source, pos, raw)
        elif char in '\r\n' and len(quote) == 1:
            return None
        else:
            pos += 1
    return None


def _ends_on_empty_line(parts):
    for token in reversed(parts):
        if token == '\n':
            return True
        if not token.isspace():
            return False
    return False


def _append_literal(parts, source, start, end, quote):
    if start < end:
        text = source[start:end].replace('{{', '{').replace('}}', '}')
        parts.append(FStringLiteralToken(quote + text + quote))


def _read_field(source, pos, quote, raw, scanner):
    pattern, token_class = scanner
    start = pos
    parts = []
    closers = []
    while pos < len(source):
        match = pattern.match(source, pos)
        token = match.group()
        pos = match.end()
        kind = match.lastgroup
        if kind == 'interpolated':
            string = _read_string(source, pos, token.lower(), scanner)
            if string is None:
                return None
            pos, tokens = string
            parts.append(match if token_class is not None else token)
            parts.extend(tokens)
            continue
        if not closers:
            if token == '}':
                return pos, _field_parts(parts, source, start, pos - 1, scanner)
            if token.startswith(':'):
                spec = _read_format(source, match.start() + 1, quote, raw, scanner)
                if spec is None:
                    return None
                end, tokens = spec
                parts.append(match if token_class is not None and token == ':' else ':')
                parts.extend(tokens)
                return end, _field_parts(parts, source, start, end - 1, scanner)
        if token in _CLOSERS:
            closers.append(_CLOSERS[token])
        elif token in (')', ']', '}'):
            if not closers or token != closers.pop():
                return None
        if kind == 'crlf_join' and token_class is None:
            token = '\\\n'
        parts.append(match if token_class is not None else token)
    return None


def _field_parts(parts, source, start, end, scanner):
    pattern, token_class = scanner
    if token_class is None:
        return parts
    # Factories receive matches against the replacement field, as before.
    field = source[start:end]
    return [part if isinstance(part, str) else
            token_class(pattern.match(field, part.start() - start)) for part in parts]


def _read_format(source, pos, quote, raw, scanner):
    # '#' and quotes are ordinary format text. Only '{' opens a field here.
    literal_start = pos
    parts = []
    while pos < len(source):
        char = source[pos]
        if char == '}':
            _append_literal(parts, source, literal_start, pos, quote)
            return pos + 1, parts
        if char == '{':
            field = _read_field(source, pos + 1, quote, raw, scanner)
            if field is None:
                return None
            _append_literal(parts, source, literal_start, pos, quote)
            pos, tokens = field
            parts.append('{')
            parts.extend(tokens)
            parts.append('}')
            literal_start = pos
        elif char == '\\':
            pos = _after_escape(source, pos, raw)
        else:
            pos += 1
    return None


def _after_escape(source, pos, raw):
    if not raw and source.startswith(r'\N{', pos):
        end = source.find('}', pos + 3)
        if end != -1:
            return end + 1
    if source.startswith('\r\n', pos + 1):
        return pos + 3
    if source[pos + 1:pos + 2] in ('\\', "'", '"', '\r', '\n'):
        return pos + 2
    # A backslash does not escape a replacement field's opening brace.
    return pos + 1
