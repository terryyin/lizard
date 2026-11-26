'''
Language parser for Scala
'''
from itertools import tee
from unicodedata import category

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
__author__ = 'David Baum'

# A line break inside a body does not end it before a token that cannot
# start a statement, or after an operator that needs a right operand.
_CONTINUES_FROM_LINE_START = frozenset((
    '.', '{', 'else', 'catch', 'finally', 'match', 'with', 'yield'))
_CONTINUES_AT_LINE_END = frozenset((
    '&&', '||', '+', '-', '*', '/', '%', '++', '::', '==', '!=', '=>',
    '<=', '>=', '<', '|', '&', '^', '->', '=', '+=', '-=', ':',
    ',', '.', 'else', 'then', 'do', 'yield', 'try', 'finally', 'match',
    'catch', 'new', 'throw'))
_CONDITION_KEYWORDS = frozenset(('if', 'while', 'for'))


class ScalaReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['scala']
    language_names = ['scala']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'do'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}  # Pattern matching
    _ternary_operators = {'?'}

    def __init__(self, context):
        self.track_leading_infix = False
        super(ScalaReader, self).__init__(context)
        self.parallel_states = [ScalaStates(context)]

    def preprocess(self, tokens):
        """Drop spaces, and offer each line's first code token to the
        function body that it may end, before any counter charges it.
        A '\n' is consumed by line_counter before the states see it."""
        if self.track_leading_infix:
            tokens = _mark_line_leading_infix(tokens)
        open_bodies = self.parallel_states[0].open_bodies
        # 0: inside a line, 1: at a line start, 2: at a line start after a
        # blank line, 3: after a comment at a line start, which is no blank.
        after_line_break = (1, 2, 2, 1)
        line_start = 0
        for token in tokens:
            if not token.isspace():
                if line_start:
                    if not token.startswith(('//', '/*')):
                        if open_bodies:
                            open_bodies[-1].end_body_before_line(
                                token, line_start == 2)
                        line_start = 0
                    elif line_start == 1:
                        line_start = 3
                elif token.startswith('/*') and '\n' in token:
                    # A block comment that ends on a later line.
                    line_start = 3
                yield token
            elif token == '\n':
                line_start = after_line_break[line_start]
                yield token


def _mark_line_leading_infix(tokens):
    """Mark an identifier whose symbolic suffix touches it at a new line."""
    tokens = iter(tokens)
    after_line_break = (1, 2, 2, 1)
    line_start = 0
    while True:
        for token in tokens:
            if token == '\n':
                line_start = after_line_break[line_start]
            elif line_start and not token.isspace():
                if token.startswith(('//', '/*')):
                    if line_start == 1:
                        line_start = 3
                else:
                    if line_start != 2 and token.endswith('_') and token.isidentifier():
                        tokens, lookahead = tee(tokens)
                        if _symbolic_suffix(next(lookahead, None)):
                            token = _LeadingInfixName(token)
                        del lookahead
                        line_start = 0
                        yield token
                        break
                    line_start = 0
            elif token.startswith('/*') and '\n' in token:
                line_start = 3
            yield token
        else:
            return


class _LeadingInfixName(str):
    """The identifier part of a Scala 3 operator such as ``and_+``."""

    is_leading_infix = True


def _symbolic_suffix(token):
    if not token or token.startswith(('//', '/*')):
        return False
    first = token[0]
    return first in '!#%&*+-/:<=>?@\\^|~' or category(first) in ('Sm', 'So')


class ScalaStates(GoLikeStates):  # pylint: disable=R0903

    FUNC_KEYWORD = 'def'

    def __init__(self, context, open_bodies=None):
        super().__init__(context)
        self._brackets = 0
        # (bracket depth, keyword) of each open `if (`, `while (`, `for (`.
        self._conditions = []
        # The keyword whose condition the last ')' closed, or None.
        self._after_condition = None
        self._open_dos = 0
        # The token before the last one in the body: `counter.+` selects a
        # member named `+`, which needs no right operand.
        self._previous = None
        # The states, outermost first, that read an expression body or own
        # an open brace body. All nested states of a file share this list.
        self.open_bodies = [] if open_bodies is None else open_bodies

    def statemachine_clone(self):
        return ScalaStates(self.context, self.open_bodies)

    def _function_name(self, token):
        if token == '`':
            self._state = self._quoted_function_name
        else:
            super()._function_name(token)

    def _quoted_function_name(self, token):
        """A name in backticks, such as `match`. As for other names, only
        its first word is kept."""
        self.context.add_to_function_name(token)
        self._state = self._rest_of_quoted_name

    def _rest_of_quoted_name(self, token):
        if token == '`':
            self._state = self._expect_function_dec

    def _expect_function_dec(self, token):
        if token in ('(', '[', '<'):
            super()._expect_function_dec(token)
        else:
            # A definition without parameters, such as `def size = 1`, is
            # not a function. Its tokens count in the enclosing function.
            self.fold_into_enclosing_function()
            # The enclosing function already counted the `def` and its line.
            self.context.current_function.nloc -= 1
            self.context.current_function.token_count -= 1
            self._state = self._state_global

    def _expect_function_impl(self, token):
        if token == "=":
            self._state = self._expect_function_body
        else:
            super(ScalaStates, self)._expect_function_impl(token)

    def _expect_function_body(self, token):
        """The body starts after '=', on the same line or the next."""
        self._brackets = 0
        self._conditions = []
        self._open_dos = 0
        self.open_bodies.append(self)
        self.next(self._function_body, token)

    def _function_body(self, token):
        self._previous = self.last_token
        if token == '{':
            # A line break inside the braces cannot end this body, so the
            # body leaves the open list until its braces close.
            self.open_bodies.pop()
            self.sub_state(self.statemachine_clone(), self._reopen_body)
        elif token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
        elif token == 'do':
            # Not the `do` of Scala 3 `while (cond) do` or `for (...) do`.
            if not (self.last_token == ')' and
                    self._after_condition in ('while', 'for')):
                self._open_dos += 1
        elif token == '}':
            # The brace closes the enclosing block, which ends on this line
            # and owns the brace.
            self.context.current_function.token_count -= 1
            self._end_function_body()
            self.context.current_function.token_count += 1
            self.context.current_function.end_line = self.context.current_line
            self.next(self._state_global, token)
        elif token == ';' and self._brackets <= 0 and not self._open_dos:
            self._end_function_body()
        elif token == 'def' and self.last_token[0] not in '"`':
            # No expression goes on into a definition. After a quote, the
            # word is text in a multi-line string, which the tokenizer
            # splits, or a name in backticks.
            self._end_function_body()
            self.next(self._state_global, token)
        elif len(token) > 1 and token[0] == '#':
            # The tokenizer reads the rest of the line after '#' as a
            # preprocessor line, so its brackets cannot be counted.
            self._brackets = 0

    def _open_bracket(self):
        if self.last_token in _CONDITION_KEYWORDS:
            if self.last_token == 'while' and self._open_dos:
                # The `while` that ends `do ... while (cond)`.
                self._open_dos -= 1
            else:
                self._conditions.append((self._brackets, self.last_token))
        self._brackets += 1

    def _close_bracket(self):
        self._brackets -= 1
        self._after_condition = None
        if self._conditions and self._brackets == self._conditions[-1][0]:
            self._after_condition = self._conditions.pop()[1]

    def _reopen_body(self):
        self.open_bodies.append(self)
        # `for { ... }` is a loop header, like `for (...)`.
        self._after_condition = 'for' if self._previous == 'for' else None

    def end_body_before_line(self, token, after_blank_line):
        """A body ends at a line break outside brackets, unless the
        expression continues on the new line."""
        if (self._state in (self._function_body,) and self._brackets <= 0
                and not self._continues_on(token, after_blank_line)):
            self._end_function_body()

    def _continues_on(self, token, after_blank_line):
        """Whether `token`, the first on a new line, continues the body."""
        last = self.last_token
        if token in _CONTINUES_FROM_LINE_START:
            # Only one line break may come before a `{` argument, and a
            # `{` after a block starts a block of its own.
            return not (token == '{' and (after_blank_line or last == '}'))
        if last in _CONTINUES_AT_LINE_END and self._previous != '.':
            return True
        if last == '>' and self._previous == '>':
            # A shift. One `>` can end an XML literal.
            return True
        if last in (')', '}') and self._after_condition:
            # After `if (cond)` or `for { ... }`, the body comes next.
            return True
        # The `while (cond)` of `do ... while (cond)`.
        return token == 'while' and self._open_dos > 0

    def _end_function_body(self):
        self.open_bodies.pop()
        self.context.end_of_function()
        self._state = self._state_global

    def statemachine_before_return(self):
        if self._state in (self._expect_function_body, self._function_body):
            self.context.end_of_function()
