"""JavaScript / TypeScript expression and ASI predicates, and JSX token types."""

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
            or (first == '<' and token.endswith('>')  # JSX element end
                and ('</' in token or token.endswith('/>'))))


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
