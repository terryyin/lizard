"""Swift condition lists.

`if a, b`, `guard a, b else` and `while a, b` evaluate their clauses
left to right and stop at the first one that fails, just like `a && b`, so
each top-level comma of such a list is a decision. Commas nested in
brackets, generic argument lists, closures and patterns are not.

A `{` ends the list when it is the statement body. A same-line trailing
closure does not: `if items.contains { $0 > 1 }, ready` still counts the
comma. A `{` that runs onto the next line is the body, matching Swift,
which only accepts a trailing closure in a condition on one line.
"""

_STARTS = ('if', 'guard', 'while')
_OPEN = ('(', '[', '{')
_CLOSE = (')', ']', '}')
# Swift reads `<...>` as generic arguments only when the token after the
# closing `>` is one of these; otherwise `<` and `>` are comparisons.
_AFTER_GENERIC_ARGUMENTS = {
    '(', ')', '[', ']', '{', '}', '.', ',', ';', ':', '?', '!', '==', '!=',
    'else'}
_IN_GENERIC_ARGUMENTS = {'.', ':', '->', '?', '!', '&', '...'}
# Tokens that keep a same-line `{...}` inside the condition expression.
_EXPRESSION_CONTINUATIONS = {
    ',', '{', '.', '(', '[', '?', '!',
    '==', '!=', '<', '>', '<=', '>=', '&&', '||', '??',
    '+', '-', '*', '/', '%', '=',
    '+=', '-=', '*=', '/=', '%=',
    '...', '..<',
}


def _is_type_name(token):
    name = token.rstrip('?!')
    return name.isidentifier() or (
        len(name) > 2 and name[0] == name[-1] == '`')


def _brace_is_body(keyword, nxt):
    if nxt == 'else':
        return keyword != 'guard'
    return nxt not in _EXPRESSION_CONTINUATIONS


class _ConditionList:
    # pylint: disable=too-few-public-methods

    def __init__(self, context, keyword):
        self._context = context
        self._keyword = keyword
        self._depth = 0
        # A `<` in the list may open generic arguments; its commas wait
        # until the closing `>` and the token after it tell which it is.
        self._angles = 0
        self._angle_depth = 0
        self._angle_commas = 0
        self._angle_closed = False
        # A `{` at depth 0 may be the body or a same-line trailing closure.
        self._brace_line = None
        self._awaiting_brace = False

    def consume(self, token, line):
        """Read one token. Return False once the list has ended."""
        if self._awaiting_brace:
            self._awaiting_brace = False
            if _brace_is_body(self._keyword, token):
                return False
        elif self._brace_line is not None and line != self._brace_line:
            self._brace_line = None
            return False
        return self._feed(token, line)

    def _feed(self, token, line):
        if self._angle_closed:
            self._angle_closed = False
            self._resolve_angle(token not in _AFTER_GENERIC_ARGUMENTS)
        elif self._angles:
            if self._in_angle(token):
                return True
            self._resolve_angle(True)
        if token == '{' and self._depth == 0:
            self._depth = 1
            self._brace_line = line
            return True
        if token in _OPEN:
            self._depth += 1
        elif token in _CLOSE:
            self._depth -= 1
            if self._depth == 0 and self._brace_line == line:
                self._brace_line = None
                self._awaiting_brace = True
                return True
            return self._depth >= 0
        elif self._depth == 0:
            if token == ';' or (token == 'else' and self._keyword == 'guard'):
                return False
            if token == ',':
                self._context.add_condition()
            elif token == '<':
                self._angles = 1
        return True

    def _in_angle(self, token):
        if token == '<':
            self._angles += 1
        elif token == '>':
            self._angles -= 1
            self._angle_closed = self._angles == 0
        elif token == ',':
            self._angle_commas += self._angle_depth == 0
        elif token in ('(', '['):
            self._angle_depth += 1
        elif token in (')', ']'):
            self._angle_depth -= 1
            return self._angle_depth >= 0
        else:
            return token in _IN_GENERIC_ARGUMENTS or _is_type_name(token)
        return True

    def _resolve_angle(self, comparison):
        if comparison:
            self._context.add_condition(self._angle_commas)
        self._angles = self._angle_depth = self._angle_commas = 0


class SwiftConditionLists:
    """Counts the clause-separating commas of Swift condition lists."""

    _STARTS = _STARTS

    def __init__(self, context):
        self.context = context
        self._lists = []
        self._previous = None
        self._repeat_braces = []
        self._closed_repeat = False

    def __call__(self, token):
        previous = self._previous
        self._previous = token
        repeat_while = self._closed_repeat
        self._closed_repeat = False
        if (self._lists or token in _STARTS or token == '{' or token == '}'):
            self._read(token, previous, repeat_while)

    def _read(self, token, previous, repeat_while):
        if self._lists:
            line = self.context.current_line
            self._lists = [
                lst for lst in self._lists if lst.consume(token, line)]
        if token == '{':
            self._repeat_braces.append(previous == 'repeat')
        elif token == '}' and self._repeat_braces:
            self._closed_repeat = self._repeat_braces.pop()
        elif token in _STARTS and previous != '.' and not (
                token == 'while' and repeat_while):
            # `repeat { } while x` takes one expression, not a list.
            self._lists.append(_ConditionList(self.context, token))
