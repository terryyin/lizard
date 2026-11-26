'''Decide which slashes open a JS/Ruby regex literal versus division.'''

import re

# A dot touches the digits of a decimal integer. A dot after a fraction,
# an exponent or an identifier names a member instead.
_DECIMAL_DOT = re.compile(r'(?:0|[1-9](?:_?[0-9])*)\.')

# One literal grammar shared with balanced template scanning; context decides
# whether a slash opens that literal or remains division.
REGEX_LITERAL = (r"/(?![/*])(?:\\[^\r\n]|\[(?:\\[^\r\n]|[^\\\]\r\n])*\]"
                 r"|[^/\\[\r\n])+/[a-z]*")
# Pre-token marker so REGEX_TOKEN needs no keyword lookbehind (tokens_in_span).
REGEX_START_MARKER = "\ue000"
# Trunk punctuation lookbehind plus marker form for pre-scanned opens.
_PUNCT_OPEN = r"(?:\A|(?<=[=,({[?:!&|;]))"
REGEX_TOKEN = (
    _PUNCT_OPEN + r"\s*" + REGEX_LITERAL
    + r"|" + r"\s*" + REGEX_START_MARKER + REGEX_LITERAL[1:]
)
# When nothing was marked, keep the trunk pattern bytes exactly.
REGEX_TOKEN_PUNCT = _PUNCT_OPEN + r"\s*" + REGEX_LITERAL
# JSX text is not code: a slash in it (`9% / 11%</p>`) must not open a
# literal that ends in the next closing tag. In JSX/TSX a literal may not end
# with an unescaped `<` whose slash starts closing-tag syntax (`</p>`, `</>`);
# `/</g` stays a literal.
_JSX_LITERAL = REGEX_LITERAL.replace(
    "+/[a-z]*",
    r"+(?:(?<!(?<!\\)<)|(?!/(?:[A-Za-z][\w.:-]*\s*)?>))/[a-z]*")
REGEX_TOKEN_JSX = (
    _PUNCT_OPEN + r"\s*" + _JSX_LITERAL
    + r"|" + r"\s*" + REGEX_START_MARKER + _JSX_LITERAL[1:]
)
REGEX_TOKEN_PUNCT_JSX = _PUNCT_OPEN + r"\s*" + _JSX_LITERAL
_REGEX_LITERAL_RE = re.compile(REGEX_LITERAL)
_EXPRESSION_KEYWORDS = frozenset({
    'return', 'typeof', 'instanceof', 'in', 'new', 'delete', 'void',
    'throw', 'case', 'do', 'else', 'yield',
})
# Ruby starts an expression after these words too: `if /^end$/ =~ s`.
_RUBY_EXPRESSION_KEYWORDS = _EXPRESSION_KEYWORDS | frozenset({
    'if', 'unless', 'elsif', 'when', 'while', 'until', 'and', 'or', 'not',
    'then', 'next', 'break',
})
_NEEDS_FORWARD = re.compile(r'\b(?:for|async)\b')
# Cheap gate from the same keyword set; longer names first so ``in`` loses to
# ``instanceof``. Skip the shared scan when no keyword/=> can open a literal.
_MAYBE_KEYWORD_OPEN = re.compile(
    r'(?<![$\w])(?:'
    + '|'.join(sorted(_EXPRESSION_KEYWORDS, key=len, reverse=True))
    + r')\b|=>')
_LINE_ENDS = frozenset('\r\n\u2028\u2029')
# A string can hold U+2028 and U+2029, so only these end one that does not close.
_STRING_LINE_ENDS = frozenset('\r\n')
_WS = frozenset(' \t\v\f\r\n\u2028\u2029')
_WORD_CONT = frozenset(
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$')
# Pure-function memo for reused source objects (warmed analyzer batches).
_MARK_CACHE = {}


def mark_regex_literal_starts(source, language='js'):
    """Mark regex-open slashes that punctuation lookbehind cannot see.

    Shared decision: a slash opens a literal where an expression can begin;
    a completed operand/member stays division. ``of`` / ``await`` use a
    forward pass when ``for`` / ``async`` appear.
    """
    key = (language, id(source))
    cached = _MARK_CACHE.get(key)
    if cached is not None and cached[0] is source:
        return cached[1]
    result = _mark_regex_literal_starts(source, language)
    _MARK_CACHE[key] = (source, result)
    return result


def _mark_regex_literal_starts(source, language):
    if '/' not in source:
        return source
    # Lazy imports keep scan modules free to import shared helpers here.
    if language == 'ruby':
        from .js_style_regex_forward_scan import mark_forward
        return mark_forward(source, js_family=False)
    if _NEEDS_FORWARD.search(source):
        from .js_style_regex_forward_scan import mark_forward
        return mark_forward(source, js_family=True)
    if _MAYBE_KEYWORD_OPEN.search(source) is None:
        return source
    from .js_style_regex_keyword_scan import mark_keyword_slashes
    return mark_keyword_slashes(source)


def _apply_marks(source, marks):
    if not marks:
        return source
    parts = []
    prev = 0
    marker = REGEX_START_MARKER
    for pos in marks:
        parts.append(source[prev:pos])
        parts.append(marker)
        prev = pos + 1
    parts.append(source[prev:])
    return ''.join(parts)


def _skip_ws_and_comments_back(source, i):
    while i >= 0:
        ch = source[i]
        if ch in _WS:
            i -= 1
            continue
        if ch == '/' and i >= 1 and source[i - 1] == '*':
            i -= 2
            while i >= 1:
                if source[i - 1] == '/' and source[i] == '*':
                    i -= 2
                    break
                i -= 1
            continue
        return i
    return i


def _word_before(source, i):
    if i < 0 or source[i] not in _WORD_CONT:
        return None, i
    end = i + 1
    while i >= 0 and source[i] in _WORD_CONT:
        i -= 1
    return source[i + 1:end], i


def _is_property_before(source, i):
    j = _skip_ws_and_comments_back(source, i)
    if j < 0 or source[j] != '.':
        return False
    start = j
    while start and source[start - 1] in '0123456789_':
        start -= 1
    if _DECIMAL_DOT.fullmatch(source, start, j + 1) is None:
        return True
    if not _separate_decimal(source, start):
        return True
    return _follows_exponent_sign(source, start)


def _separate_decimal(source, start):
    if not start:
        return True
    before = source[start - 1]
    if before.isalnum() or before in '_$.':
        return False
    if not before.isascii():
        return before.isspace() or before == '\ufeff'
    if before == '}':
        # An escape at the end of an identifier can precede these digits.
        escape = start - 2
        while escape >= 0 and source[escape] in '0123456789abcdefABCDEF':
            escape -= 1
        if escape < start - 2 and escape >= 2 and source.startswith(r'\u{', escape - 2):
            return False
    return True


def _follows_exponent_sign(source, start):
    if start < 3 or source[start - 1] not in '+-' or source[start - 2] not in 'eE':
        return False
    mantissa = start - 2
    while mantissa and source[mantissa - 1] in '0123456789_.':
        mantissa -= 1
    return mantissa < start - 2 and _separate_decimal(source, mantissa)
