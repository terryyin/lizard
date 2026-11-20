"""Where JavaScript ends a statement without a ';'."""

from .javascript_functions import JSXChildBrace, ends_expression, starts_statement

# Structures with a head in parentheses before their body.
HEADED = frozenset(('if', 'for', 'while', 'with', 'switch', 'catch'))
# Structures with their body directly after the word.
UNHEADED = frozenset(('else', 'do', 'try', 'finally'))
OPENERS = frozenset(('(', '['))
CLOSERS = frozenset((')', ']'))
TRACKED = HEADED | UNHEADED | OPENERS | CLOSERS | {'{', '${', '}', 'await'}


class ImpliedSemicolons:
    """Finds the places where a statement ends without a ';'.

    Ask before(token) once for each token, in order.  A statement ends
    before a '}' and at a line break that the next token cannot continue.
    It does not end inside parentheses or brackets, or while a structure
    waits for its body, `if (ready)` + line break + `run()`."""

    def __init__(self, context):
        self._context = context
        self._blocks = [_Block()]
        self._previous = ';'
        self._line = 0
        self._head_starts = False       # the next "(" opens a structure head
        self._body_starts = False       # a structure waits for its body
        self._loop_ended = False        # the ")" of "do … while (…)"

    def before(self, token):
        line = self._context.current_line
        implied = self._loop_ended and token != ';' or (
            (token == '}' or line != self._line)
            and self._statement_ends(token, line))
        if (token in TRACKED or self._head_starts or self._body_starts
                or self._loop_ended):
            self._read(token)
        self._previous = token
        self._line = line
        return implied

    def _statement_ends(self, token, line):
        if token != '}' and (line - token.count('\n') == self._line
                             or not starts_statement(token)
                             or isinstance(token, JSXChildBrace)):
            return False
        previous = self._previous
        # A block end has ended its statement already.
        return not (self._body_starts or self._blocks[-1].groups
                    or previous == '}' or not ends_expression(previous))

    def _read(self, token):
        block = self._blocks[-1]
        head_starts, self._head_starts = self._head_starts, False
        self._body_starts = self._loop_ended = False
        if token in OPENERS:
            if head_starts and token == '(':
                block.head = block.groups
            block.groups += 1
        elif token in CLOSERS:
            self._close_group(block)
        elif token == '}':
            if len(self._blocks) > 1:
                self._blocks.pop()
        elif token in ('{', '${'):
            self._blocks.append(_Block(token))
        elif token in TRACKED and self._previous not in ('.', '?.'):
            self._read_word(token, block, head_starts)

    def _close_group(self, block):
        if block.groups:
            block.groups -= 1
        if block.head == block.groups:
            block.head = None
            self._loop_ended = block.loop_ends
            self._body_starts = not block.loop_ends
            block.loop_ends = False

    def _read_word(self, token, block, head_starts):
        if token in HEADED:
            self._head_starts = True
            if token == 'while' and block.open_loops:
                block.open_loops -= 1
                block.loop_ends = True
        elif token in UNHEADED:
            self._body_starts = True
            if token == 'do':
                block.open_loops += 1
        elif token == 'await':          # for await (…)
            self._head_starts = head_starts


class _Block:
    """What is open between a '{' and its '}'."""

    __slots__ = ('groups', 'head', 'open_loops', 'loop_ends')

    def __init__(self, opener='{'):
        # The braces of a JSX child or a template interpolation hold an
        # expression, as parentheses do.
        expression = opener == '${' or isinstance(opener, JSXChildBrace)
        self.groups = 1 if expression else 0    # open "(" and "["
        self.head = None        # the groups around an open structure head
        self.open_loops = 0     # "do" bodies that wait for their "while"
        self.loop_ends = False  # the open head is the "while" of a "do"
