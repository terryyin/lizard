"""Python tokens and f-string interpolation expressions."""

from .token_pattern import compiled_token_pattern, tokens_in_span


class FStringExpressionToken(str):
    '''An f-string token whose delimiters belong to the interpolation.'''


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
_PY_ADDITION = r"|\#[^\n]*|//=?" + _PY_TRIPLE_QUOTE
_CLOSERS = {'(': ')', '[': ']', '{': '}'}


def generate_tokens(source_code, addition='', token_class=None, *, interpolate=True):
    if not interpolate:
        pattern = compiled_token_pattern(_PY_ADDITION + addition)
        return _mark_prefixed_strings(
            tokens_in_span(pattern, source_code, 0, len(source_code), token_class))
    pattern = compiled_token_pattern(_PY_ADDITION + _INTERPOLATED_START + addition)
    return _mark_prefixed_strings(_source_tokens(source_code, pattern, token_class))


def _source_tokens(source, pattern, token_class):
    start = 0
    while True:
        for match in pattern.finditer(source, start):
            token = match.group()
            value = token_class(match) if token_class else token
            if match.lastgroup == 'interpolated' and isinstance(value, str):
                string = _read_string(source, match.end(), token.lower(), pattern)
                if string is not None:
                    start, parts = string
                    yield value
                    yield from parts
                    break
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


def _read_string(source, start, prefix, pattern):
    quote = source[start:start + 3]
    if quote not in ('"""', "'" * 3):
        quote = quote[0]
    pos = literal_start = start + len(quote)
    parts = []
    raw = 'r' in prefix
    while pos < len(source):
        if source.startswith(quote, pos):
            end = pos + len(quote)
            if not parts or 't' in prefix:
                return end, [PrefixedStringToken(source[start:end])]
            _append_literal(parts, source, literal_start, pos, quote)
            return end, parts
        char = source[pos]
        if source[pos:pos + 2] in ('{{', '}}'):
            pos += 2
        elif char == '{':
            field = _read_field(source, pos + 1, quote, raw, pattern)
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


def _append_literal(parts, source, start, end, quote):
    if start < end:
        text = source[start:end].replace('{{', '{').replace('}}', '}')
        parts.append(FStringLiteralToken(quote + text + quote))


def _read_field(source, pos, quote, raw, pattern):
    parts = []
    closers = []
    while pos < len(source):
        match = pattern.match(source, pos)
        token = match.group()
        pos = match.end()
        if match.lastgroup == 'interpolated':
            string = _read_string(source, pos, token.lower(), pattern)
            if string is None:
                return None
            pos, tokens = string
            parts.append(token)
            parts.extend(tokens)
            continue
        if not closers:
            if token == '}':
                return pos, parts
            if token.startswith(':'):
                spec = _read_format(source, match.start() + 1, quote, raw, pattern)
                if spec is None:
                    return None
                end, tokens = spec
                parts.append(':')
                parts.extend(tokens)
                return end, parts
        if token in _CLOSERS:
            closers.append(_CLOSERS[token])
        elif token in (')', ']', '}'):
            if not closers or token != closers.pop():
                return None
        parts.append(token)
    return None


def _read_format(source, pos, quote, raw, pattern):
    # '#' and quotes are ordinary format text. Only '{' opens a field here.
    literal_start = pos
    parts = []
    while pos < len(source):
        char = source[pos]
        if char == '}':
            _append_literal(parts, source, literal_start, pos, quote)
            return pos + 1, parts
        if char == '{':
            field = _read_field(source, pos + 1, quote, raw, pattern)
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
