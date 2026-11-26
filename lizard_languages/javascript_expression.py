"""JavaScript / TypeScript expression and ASI predicates, and JSX token types."""

from unicodedata import category

# Words that still need an operand, so a line break after them continues.
OPERAND_WORDS = frozenset((
    'in', 'instanceof', 'typeof', 'new', 'await', 'void', 'delete', 'yield',
    'of', 'as', 'satisfies', 'keyof'))


class JSXChildBrace(str):
    """The '{' of a JSX child expression, `<p>{x && <a/>}</p>`: an
    expression container, never an object literal or a block."""


class JSXMarkup(str):
    """The tags and the text of a JSX element, `<p>{name}({count})</p>`:
    never code, also when the text is a bracket or a keyword."""

    is_jsx_markup = True


class TypeArgumentEnd(str):
    """The end of TypeScript type arguments at a statement boundary."""


_OPERAND_STARTS = frozenset(('_', '$', '#', "'", '"', '`'))
_STATEMENT_STARTS = frozenset(('_', '$', '#', "'", '"'))
_EXPRESSION_ENDS = frozenset((')', ']', '}', '++', '--'))
_LINE_START_OPERATORS = frozenset((
    '++', '--', '!', '~', '{', '@', '}', ')', ']'))


def ends_expression(token):
    """Whether an expression may end with this token (JavaScript ASI)."""
    first = token[:1]
    if first.isalnum() or first in _OPERAND_STARTS:
        return token not in OPERAND_WORDS
    return (token in _EXPRESSION_ENDS
            or (first == '/' and len(token) > 2)  # regular expression literal
            or (first == '>' and isinstance(token, TypeArgumentEnd))
            or (first == '<' and token.endswith('>')  # JSX element end
                and ('</' in token or token.endswith('/>')
                     or isinstance(token, TypeArgumentEnd))))


def starts_statement(token):
    """Whether this line-start token cannot continue the previous line."""
    first = token[0]
    if first.isalnum() or first in _STATEMENT_STARTS:
        # TypeScript reads `as`/`satisfies` after a line break as a statement.
        return token not in ('in', 'instanceof')
    # Restricted ++/--, prefix-only operators, blocks, decorators and closers.
    return token in _LINE_START_OPERATORS


def parameter_group(prefix):
    return (prefix is None or prefix in ('return', 'yield', 'await', 'async')
            or (not ends_expression(prefix) and prefix not in ('.', '?.', 'new')))


def active_js_state(state):
    while isinstance(state._state, state.__class__):
        state = state._state
    return state


def awaiting_function_body(state):
    active = active_js_state(state)
    return active._state == active._expecting_func_opening_bracket


def end_before_line_token(state, arrows, token):
    """Offer a line's first token before counters charge it."""
    if arrows:
        arrows[-1].end_expression_before(token)
    else:
        active = active_js_state(state)
        if active._state == active._expecting_func_opening_bracket:
            active.end_expression_before(token)


_ID_START_CATEGORIES = frozenset(('Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl'))
_ID_PART_CATEGORIES = _ID_START_CATEGORIES | frozenset(('Mn', 'Mc', 'Nd', 'Pc'))
_OTHER_ID_START = frozenset('$_\u1885\u1886\u2118\u212e\u309b\u309c')
_OTHER_ID_PART = _OTHER_ID_START | frozenset(
    '\u00b7\u0387\u1369\u136a\u136b\u136c\u136d\u136e\u136f\u1370\u1371'
    '\u19da\u200c\u200d\u30fb\uff65')


def is_identifier(token):
    return (token.replace('$', '_').isidentifier()
            or ((token[0] in _OTHER_ID_START
                 or token[0] != '\u2e2f' and category(token[0]) in _ID_START_CATEGORIES)
                and is_identifier_part(token[1:])))


def is_identifier_part(token):
    # ECMAScript uses ID_Continue, while Python uses the normalized subset
    # XID_Continue. The vertical tilde is excluded by Pattern_Syntax.
    return (('_' + token.replace('$', '_')).isidentifier()
            or all(ch in _OTHER_ID_PART or ch != '\u2e2f' and category(ch) in _ID_PART_CATEGORIES
                   for ch in token))
