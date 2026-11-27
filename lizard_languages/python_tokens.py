"""Python tokens and f-string interpolation expressions."""

from re import Match

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
_CRLF_JOIN = r"|(?P<crlf_join>\\\r\n)"
_PY_ADDITION = r"|\#[^\n]*|//=?" + _PY_TRIPLE_QUOTE
_CLOSERS = {'(': ')', '[': ']', '{': '}'}


def generate_tokens(source_code, addition='', token_class=None, *, interpolate=True):
    if not interpolate:
        pattern = compiled_token_pattern(_PY_ADDITION + addition)
        return _mark_prefixed_strings(
            tokens_in_span(pattern, source_code, 0, len(source_code), token_class))
    if token_class is not None:
        pattern = compiled_token_pattern(_PY_ADDITION + addition)
        return _mark_prefixed_strings(_source_factory_tokens(source_code, pattern, token_class))
    pattern = compiled_token_pattern(_PY_ADDITION + _INTERPOLATED_START + _CRLF_JOIN + addition)
    return _mark_prefixed_strings(_source_tokens(source_code, pattern))


def _source_tokens(source, pattern):
    start = 0
    scanner = (pattern, None)
    while True:
        for match in pattern.finditer(source, start):
            token = match.group()
            kind = match.lastgroup
            if kind == 'interpolated':
                string = _read_string(source, match.end(), token.lower(), scanner)
                if string is not None:
                    start, parts = string
                    yield token
                    yield from parts
                    break
            elif kind == 'crlf_join':
                token = '\\\n'
            yield token
        else:
            return


def _source_factory_tokens(source, pattern, token_class):
    field_pattern = compiled_token_pattern(_PY_ADDITION + _INTERPOLATED_START + _CRLF_JOIN)
    start = 0
    factory = token_class
    while True:
        matches = pattern.finditer(source, start)
        for match in matches:
            value = factory(match)
            if isinstance(value, str) and source[match.end():match.end() + 1] in ('"', "'"):
                string = _read_factory_string(
                    source, match.end(), value.lower(), (field_pattern, factory), matches)
                if string is not None:
                    start, parts, matched_quote = string
                    yield value
                    yield from parts
                    if matched_quote:
                        continue
                    # Only a corrected quote boundary needs a new scan.
                    if factory is token_class:
                        factory = _factory_with_original_matches(source, matches, token_class)
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


def _read_factory_string(source, start, prefix, scanner, matches=None):
    if prefix not in _FSTRING_PREFIXES and prefix not in ('t', 'tr', 'rt'):
        return None
    pattern, token_class = scanner
    raw = _read_string(source, start, prefix, (pattern, None))
    if raw is None:
        return None
    end, _ = raw
    match = pattern.match(source, start) if matches is None else next(matches)
    if match.span() != (start, end):
        # Keep an explicit addition's match when the built-in regex agrees with the lexer.
        if matches is None or pattern.match(source, start).end() != end:
            _, parts = _read_string(source, start, prefix, scanner)
            return end, parts, False
        end = match.end()
    # The original factory also transformed the complete quoted token.
    text = token_class(match)
    if isinstance(text, str) and text[:1] in ('"', "'"):
        cooked = _read_string(text, 0, prefix, scanner)
        if cooked is not None and cooked[0] == len(text):
            return end, cooked[1], True
    return end, [text], True


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
            return end, parts
        char = source[pos]
        if source[pos:pos + 2] in ('{{', '}}'):
            pos += 2
        elif char == '{':
            field_start = pos + 1
            field = _read_field(source, field_start, quote, raw, scanner)
            if field is None:
                return None
            _append_literal(parts, source, literal_start, pos, quote)
            pos, tokens = field
            if scanner[1] is not None:
                tokens = _field_parts(tokens, source, field_start, pos - 1, scanner)
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


def _read_field(source, pos, quote, raw, scanner):
    pattern, token_class = scanner
    parts = []
    closers = []
    while pos < len(source):
        match = pattern.match(source, pos)
        token = match.group()
        pos = match.end()
        kind = match.lastgroup
        if kind == 'interpolated':
            if token_class is None:
                parts.append(token)
                result = _read_string(source, pos, token.lower(), scanner)
            else:
                result = _defer_nested_string(source, match, pattern)
            if result is None:
                return None
            pos, tokens = result
            parts.extend(tokens)
            continue
        if (token_class is not None and token.lower() in _STRING_PREFIXES and
                source[pos:pos + 1] in ('"', "'")):
            quoted = pattern.match(source, pos)
            if quoted.end() > pos + 1:
                parts.append(_NestedFactoryString(match.start(), quoted.end()))
                pos = quoted.end()
                continue
        if not closers:
            if token == '}':
                return pos, parts
            if token.startswith(':'):
                result = _read_format(source, match, quote, raw, scanner)
                if result is None:
                    return None
                pos, tokens = result
                parts.append(match if token_class is not None else ':')
                parts.extend(tokens)
                return pos, parts
        if token in _CLOSERS:
            closers.append(_CLOSERS[token])
        elif token in (')', ']', '}'):
            if not closers or token != closers.pop():
                return None
        if kind == 'crlf_join' and token_class is None:
            token = '\\\n'
        parts.append(match if token_class is not None else token)
    return None


def _defer_nested_string(source, match, pattern):
    result = _read_string(source, match.end(), match.group().lower(), (pattern, None))
    if result is None:
        return None
    end, _ = result
    return end, [_NestedFactoryString(match.start(), end)]


def _field_parts(parts, source, start, end, scanner):
    pattern, token_class = scanner
    # Replay nested format fields against the same source as the outer field.
    field = source[start:end]
    original_pattern = compiled_token_pattern(_PY_ADDITION)
    field_factory = _factory_with_original_matches(field, original_pattern.finditer(field), token_class)
    field_scanner = pattern, field_factory
    for part in parts:
        if isinstance(part, Match):
            yield field_factory(pattern.match(field, part.start() - start))
        elif isinstance(part, _NestedFactoryString):
            yield from part.render(field, start, field_scanner)
        else:
            yield from _format_factory_tokens(field, part.start - start, part.stop - start, field_scanner)


def _factory_with_original_matches(source, matches, token_class):
    original = next(matches, None)

    def factory(match):
        nonlocal original
        if match.string is source:
            while original is not None and original.start() < match.start():
                original = next(matches, None)
            # Keep the original scan origin when the token boundaries agree.
            if original is not None and original.span() == match.span():
                match = original
        return token_class(match)

    return factory


def _format_factory_tokens(field, start, end, scanner):
    pattern, token_class = scanner
    for match in pattern.finditer(field, start):
        if match.start() >= end:
            return
        if match.end() > end:
            # A format fill or flag cannot consume the following field.
            for bounded in pattern.finditer(field, match.start(), end):
                if bounded.start() == end:
                    return
                yield token_class(bounded)
            return
        yield token_class(match)


class _NestedFactoryString:
    """Defer a nested string until the enclosing field's offsets are known."""

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def render(self, field, start, scanner):
        pattern, token_class = scanner
        end = self.end - start
        prefix_match = pattern.match(field, self.start - start)
        prefix = token_class(prefix_match)
        yield prefix
        if isinstance(prefix, str):
            result = _read_factory_string(field, prefix_match.end(), prefix.lower(), scanner)
            if result is not None and result[0] == end:
                yield from result[1]
                return
        yield from tokens_in_span(pattern, field, prefix_match.end(), end, token_class)


def _read_format(source, delimiter, quote, raw, scanner):
    # '#' and quotes are ordinary format text. Only '{' opens a field here.
    token_class = scanner[1]
    pos = delimiter.start() + 1
    literal_start = pos if token_class is None else delimiter.end()
    parts = []
    while pos < len(source):
        char = source[pos]
        if char in '{}':
            if token_class is None:
                _append_literal(parts, source, literal_start, pos, quote)
            elif literal_start < pos:
                parts.append(slice(literal_start, pos))
            if char == '}':
                return pos + 1, parts
            field = _read_field(source, pos + 1, quote, raw, scanner)
            if field is None:
                return None
            parts.append('{' if token_class is None else slice(pos, pos + 1))
            pos, tokens = field
            parts.extend(tokens)
            parts.append('}' if token_class is None else slice(pos - 1, pos))
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
