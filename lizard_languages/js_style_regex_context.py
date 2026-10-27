'''Decide which slashes open a JS/Ruby regex literal versus division.'''

import re

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
_REGEX_LITERAL_RE = re.compile(REGEX_LITERAL)
_EXPRESSION_KEYWORDS = frozenset({
    'return', 'typeof', 'instanceof', 'in', 'new', 'delete', 'void',
    'throw', 'case', 'do', 'else', 'yield',
})
_NEEDS_FORWARD = re.compile(r'\b(?:for|async)\b')
# Cheap gate from the same keyword set; longer names first so ``in`` loses to
# ``instanceof``. Skip the shared scan when no keyword/=> can open a literal.
_MAYBE_KEYWORD_OPEN = re.compile(
    r'(?<![$\w])(?:'
    + '|'.join(sorted(_EXPRESSION_KEYWORDS, key=len, reverse=True))
    + r')\b|=>')
_LINE_ENDS = frozenset('\r\n\u2028\u2029')
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
    return j >= 0 and source[j] == '.'
