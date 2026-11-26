'''Decide which slashes open a JS/Ruby regex literal versus division.'''

import re

# One literal grammar shared with balanced template scanning; context decides
# whether a slash opens that literal or remains division.
REGEX_LITERAL = (r"/(?![/*])(?:\\[^\r\n]|\[(?:\\[^\r\n]|[^\\\]\r\n])*\]"
                 r"|[^/\\[\r\n])+/[a-z]*")
# Pre-token marker so REGEX_TOKEN needs no keyword lookbehind (tokens_in_span).
REGEX_START_MARKER = "\ue000"
# Trunk punctuation lookbehind plus marker form for pre-scanned opens.
_EXPRESSION_PUNCTUATION = '=,({[?:!&|;'
_PUNCT_OPEN = rf"(?:\A|(?<=[{_EXPRESSION_PUNCTUATION}]))"
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
# Match an operator before an operand. Compound assignments and arrows can
# precede yield, as can a single question mark in a conditional expression.
_OPERAND_OPERATOR = re.compile(r'(?:[=!]=|(?<!<)<=|(?<!>)>=|(?<!=)>|\?\?|[<+*/%~!&|^-])\Z')


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
_LINE_TERMINATORS = '\r\n\u2028\u2029'
_LINE_ENDS = frozenset(_LINE_TERMINATORS)
# Keep a divisor's trivia in the lexical span where it belongs.
_DIVISOR_LEAD = rf'/(?:\s|/\*(?:[^*]|\*(?!/))*\*/|//[^{_LINE_TERMINATORS}]*[{_LINE_TERMINATORS}])+'
_REGEX_DIVISOR = re.compile(_DIVISOR_LEAD + '(' + REGEX_LITERAL + ')')
_JS_OPERATOR_SLASH = re.compile(r'(?:[<>]\s*|' + _DIVISOR_LEAD + r')/(?![/*])')
_JS_SPACES = (' \t\v\f\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005'
              '\u2006\u2007\u2008\u2009\u200a\u202f\u205f\u3000\ufeff')
_JS_WHITESPACE = _LINE_ENDS | frozenset(_JS_SPACES)
_BASIC_WHITESPACE = _LINE_ENDS | frozenset(' \t\v\f')
# A slash after a block comment, or at the start of a line after a line comment.
_MAYBE_COMMENT_OPEN = re.compile(
    rf'\*/[{_JS_SPACES}{_LINE_TERMINATORS}]*/|[{_LINE_TERMINATORS}][{_JS_SPACES}]*/(?![/*])')
# A string can hold U+2028 and U+2029, so only these end one that does not close.
_STRING_LINE_ENDS = frozenset('\r\n')
_WORD_CONT = frozenset(
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$')
_HEX = frozenset('0123456789abcdefABCDEF')
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
    jsx = language == 'jsx'
    javascript_operators = language == 'javascript' and _JS_OPERATOR_SLASH.search(source) is not None
    if _NEEDS_FORWARD.search(source):
        from .js_style_regex_forward_scan import mark_forward
        return mark_forward(source, js_family=True, jsx=jsx, javascript_operators=javascript_operators)
    if (not javascript_operators and _MAYBE_KEYWORD_OPEN.search(source) is None
            and (jsx or _MAYBE_COMMENT_OPEN.search(source) is None)):
        return source
    from .js_style_regex_keyword_scan import mark_keyword_slashes
    return mark_keyword_slashes(source, jsx=jsx, javascript_operators=javascript_operators)


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


def _is_js_dividend(source, prev, word):
    if prev < 0 or word in ('await', 'default', 'extends'):
        return False
    operand = source[prev]
    return operand.isalnum() or operand in '_$]"`\''


def _yield_after_operator(source, start):
    prev = _skip_ws_and_comments_back(source, start - 1)
    if prev < 0 or _OPERAND_OPERATOR.search(source, max(0, prev - 1), prev + 1) is None:
        return False
    # A second yield can be the expression delegated by yield*.
    if source[prev] == '*':
        word, _ = _word_before(source, _skip_ws_and_comments_back(source, prev - 1))
        if word == 'yield':
            return False
    return True


def _opens_control_header(source, start):
    prev = _skip_ws_and_comments_back(source, start - 1)
    word, before = _word_before(source, prev)
    if word not in ('if', 'while', 'for', 'with'):
        return False
    prefix = _skip_ws_and_comments_back(source, before)
    return prefix < 0 or source[prefix] not in '.#'


def _skip_ws_back(source, i):
    while i >= 0 and source[i] in _JS_WHITESPACE:
        i -= 1
    return i


def _word_before(source, i, *, unicode_names=True):
    if i < 0 or source[i] not in _WORD_CONT:
        return None, i
    end = i + 1
    while i >= 0 and source[i] in _WORD_CONT:
        i -= 1
    if unicode_names and i >= 0:
        prefix = source[i]
        # A keyword spelling must not be the suffix of a Unicode name.
        if not prefix.isascii() and not prefix.isspace() and prefix != '\ufeff':
            return None, i
        if prefix == '}':
            opening = i - 1
            while opening >= 2 and source[opening] in _HEX:
                opening -= 1
            if opening >= 2 and source.startswith(r'\u{', opening - 2):
                return None, i
    return source[i + 1:end], i


def _skip_ws_and_comments_back(source, i):
    while i >= 0:
        ch = source[i]
        if ch in _BASIC_WHITESPACE:
            i -= 1
        elif ch == '/' and i >= 1 and source[i - 1] == '*':
            i -= 2
            while i >= 1:
                if source[i - 1] == '/' and source[i] == '*':
                    i -= 2
                    break
                i -= 1
        else:
            break
    return i


def _is_property_before(source, i):
    j = _skip_ws_and_comments_back(source, i)
    return j >= 0 and source[j] == '.'
