"""Where JavaScript ends a statement without a ';'."""

from .javascript_expression import JSXChildBrace, ends_expression, starts_statement

# Structures with a head in parentheses before their body.
HEADED = frozenset(('if', 'for', 'while', 'with', 'switch', 'catch'))
# Structures with their body directly after the word.
UNHEADED = frozenset(('else', 'do', 'try', 'finally'))
OPENERS = frozenset(('(', '['))
CLOSERS = frozenset((')', ']'))
# Operators that can come after their operand: `count++`, `item!`.
POSTFIX = frozenset(('++', '--', '!'))
TRACKED = (HEADED | UNHEADED | OPENERS | CLOSERS | POSTFIX
           | {'{', '${', '}', 'await'})
# Words that a line break after them always ends: `return` + line break.
RESTRICTED = frozenset(('return', 'break', 'continue', 'yield'))
# Words that need more after them, so a line break after them continues.
UNFINISHED = frozenset((
    'var', 'let', 'const', 'case', 'function', 'class', 'extends'))
# Words that are not a value, so a "!" or a "++" after them is a prefix.
NOT_VALUES = RESTRICTED | UNFINISHED | {'throw'}
MEMBER_ACCESS = ('.', '?.')
# Words with a value after them, so a "{" after them opens an object.
VALUE_WORDS = frozenset(('return', 'throw', 'default', 'var', 'let', 'const'))
# A "{" after these opens a block, although they do not end an expression.
# So does a "{" after a ">": `() => {`, `Promise<void> {`.
BLOCK_PREFIXES = frozenset((';', '{', 'void'))
# TypeScript: these words are a complete type after `as`, `pair as const`.
ASSERTIONS = ('as', 'satisfies')
ASSERTED_TYPES = ('const', 'void')


class ImpliedSemicolons:
    """Finds the places where a statement ends without a ';'.

    Ask before(token) once for each token, in order.  A statement ends
    before the '}' of a block and at a line break that the next token
    cannot continue.  It does not end inside parentheses, brackets or an
    object literal, or while a structure waits for its head or its body,
    `if (ready)` + line break + `run()`."""

    def __init__(self, context):
        self._context = context
        self._blocks = [_Block()]
        self._previous = ';'
        self._before_previous = ';'
        self._line = 0
        self._head_starts = False       # the next "(" opens a structure head
        self._body_starts = False       # a structure waits for its body
        self._label_starts = False      # the body may start with a label
        self._postfix = False           # the last "!", "++" or "--" is postfix
        self._object_ended = False      # the last "}" closed an object literal
        self._loop_ended = False        # the ")" of "do … while (…)"

    def before(self, token):
        line = self._context.current_line
        implied = self._loop_ended and token != ';' or (
            (token == '}' or line != self._line)
            and self._statement_ends(token, line))
        if (token in TRACKED or self._head_starts or self._body_starts
                or self._label_starts or self._loop_ended):
            self._read(token, line)
        self._before_previous = self._previous
        self._previous = token
        self._line = line
        return implied

    def _statement_ends(self, token, line):
        if self._head_starts or self._body_starts or self._blocks[-1].groups:
            return False
        # A block end has ended its statement already.
        if self._previous == '}' and not self._object_ended:
            return False
        if token != '}':
            if (line - token.count('\n') == self._line
                    or isinstance(token, JSXChildBrace)):
                return False
            if self._previous in RESTRICTED and not self._after_member_access():
                return token != ';'
            if not starts_statement(token):
                return False
        return self._expression_ended()

    def _expression_ended(self):
        previous = self._previous
        if self._after_member_access():     # a property name: `token.in`
            return True
        if previous in POSTFIX:
            return self._postfix
        if previous in ASSERTED_TYPES and self._before_previous in ASSERTIONS:
            return True
        return previous not in UNFINISHED and ends_expression(previous)

    def _value_ended(self):
        """Whether an operator after the previous token is a postfix one."""
        previous = self._previous
        if previous in POSTFIX:             # TypeScript: `item.owner!!`
            return self._postfix
        return self._after_member_access() or (
            previous != '}' and previous not in NOT_VALUES
            and ends_expression(previous))

    def _after_member_access(self):
        return self._before_previous in MEMBER_ACCESS

    def _read(self, token, line):
        block = self._blocks[-1]
        head_starts, self._head_starts = self._head_starts, False
        body_starts, self._body_starts = self._body_starts, False
        label_starts, self._label_starts = self._label_starts, False
        self._loop_ended = False
        if token in OPENERS:
            if head_starts and token == '(':
                block.head = block.groups
            block.groups += 1
        elif token in CLOSERS:
            self._close_group(block)
        elif token == '}':
            self._object_ended = (len(self._blocks) > 1
                                  and self._blocks.pop().is_object)
        elif token in ('{', '${'):
            self._blocks.append(
                _Block(token, self._object_starts(token, block, line)))
        elif token in POSTFIX:
            self._postfix = (line == self._line and self._value_ended()
                             and not (head_starts or body_starts))
        elif token in TRACKED and self._previous not in MEMBER_ACCESS:
            self._read_word(token, block, head_starts, body_starts)
        elif body_starts:
            self._label_starts = True
        elif label_starts and token == ':':  # `do retry: while (…) …`
            self._body_starts = True

    def _object_starts(self, token, block, line):
        previous = self._previous
        if token != '{' or isinstance(token, JSXChildBrace):
            return False
        if previous == ':':
            # A property value or a branch of "?:", not a `case` or a label.
            return block.groups > 0
        if self._after_member_access() and previous not in OPENERS:
            return False                # a property name: `typeof Array.of {`
        if previous in VALUE_WORDS:
            return line == self._line       # not `return` + line break
        return not (previous in BLOCK_PREFIXES or previous.endswith('>')
                    or ends_expression(previous))

    def _close_group(self, block):
        if block.groups:
            block.groups -= 1
        if block.head == block.groups:
            block.head = None
            self._loop_ended = block.loop_ends
            self._body_starts = not block.loop_ends
            block.loop_ends = False

    def _read_word(self, token, block, head_starts, body_starts):
        if token in HEADED:
            self._head_starts = True
            # In `do while (a) run(); while (b)` the first "while" is the body.
            if token == 'while' and block.open_loops and not body_starts:
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

    __slots__ = ('groups', 'head', 'open_loops', 'loop_ends', 'is_object')

    def __init__(self, opener='{', is_object=False):
        # The braces of an object literal, a JSX child or a template
        # interpolation hold an expression, as parentheses do.
        expression = (is_object or opener == '${'
                      or isinstance(opener, JSXChildBrace))
        self.is_object = is_object
        self.groups = 1 if expression else 0    # open "(" and "["
        self.head = None        # the groups around an open structure head
        self.open_loops = 0     # "do" bodies that wait for their "while"
        self.loop_ends = False  # the open head is the "while" of a "do"
