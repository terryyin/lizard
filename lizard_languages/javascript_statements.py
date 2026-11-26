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
MEMBER_ACCESS = frozenset(('.', '?.'))
# Words with a value after them, so a "{" after them opens an object.
VALUE_WORDS = frozenset(('return', 'throw', 'default', 'var', 'let', 'const'))
# A "{" after these opens a block, although they do not end an expression.
# So does a "{" after a ">": `() => {`, `Promise<void> {`.
BLOCK_PREFIXES = frozenset((';', '{', 'void'))
# TypeScript: these words are a complete type after `as`, `pair as const`.
ASSERTIONS = frozenset(('as', 'satisfies'))
ASSERTED_TYPES = frozenset(('const', 'void'))
# The tokens with their own rule for the end of an expression.
_SPECIAL_ENDS = POSTFIX | ASSERTED_TYPES | UNFINISHED


def opens_object(previous):
    """Whether a "{" after this token opens an object literal, `x = {`.
    After ":" it can also be a `case` body."""
    return previous in VALUE_WORDS or not (
        previous in BLOCK_PREFIXES or previous.endswith('>')
        or ends_expression(previous))


class ImpliedSemicolons:
    """Finds the places where a statement ends without a ';'.

    read(tokens, context) passes each token on, in order.  While a token
    is passed on, before_token tells whether a statement ends before it.
    A statement ends before the '}' of a block and at a line break that
    the next token cannot continue.  It does not end inside parentheses,
    brackets or an object literal, or while a structure waits for its
    head or its body, `if (ready)` + line break + `run()`."""

    __slots__ = ('before_token',)

    def __init__(self):
        self.before_token = False

    # pylint: disable=R0912,R0914,R0915,R1702
    def read(self, tokens, context):
        context.closed_class_expression = False
        # Each token passes here, so the state is in local variables.
        blocks = []             # the blocks around the open one
        # What is open between the last "{" and its "}":
        groups = 0              # open "(" and "["
        head = None             # the groups around an open structure head
        open_loops = 0          # "do" bodies that wait for their "while"
        loop_ends = False       # the open head is the "while" of a "do"
        is_object = False       # the "{" opened an object literal
        previous = before_previous = ';'
        last_line = 0
        head_starts = False     # the next "(" opens a structure head
        body_starts = False     # a structure waits for its body
        label_starts = False    # the body may start with a label
        postfix = False         # the last "!", "++" or "--" is postfix
        object_ended = False    # the last "}" closed an object literal
        last_closed_body = context.last_closed_function_body = None
        loop_ended = False      # the ")" of "do … while (…)"
        found = False
        busy = False            # found, or one of the four flags above
        # The flags that the last token left: only a busy token reads them.
        head_started = body_started = label_started = False
        for token in tokens:
            line = context.current_line
            tracked = token in TRACKED
            if tracked or busy or line != last_line:
                if busy or line != last_line or token == '}':
                    if (previous == '}' and context.closed_class_expression
                            and token != 'else' and not (token == 'while' and open_loops)):
                        object_ended = True
                    if found:
                        found = self.before_token = False
                    if loop_ended:
                        found = token != ';'
                    elif groups or head_starts or body_starts:
                        pass
                    elif (previous != '}' or object_ended or (
                            _function_closed(context, last_closed_body)
                            # A matching clause continues the structure.
                            and token != 'else'
                            and not (token == 'while' and open_loops))):
                        if token == '}':
                            found = _expression_ended(
                                previous, before_previous, postfix)
                        elif line == last_line:
                            pass
                        elif (previous in RESTRICTED
                              and before_previous not in MEMBER_ACCESS):
                            found = token != ';' and _starts_line(
                                token, line, last_line)
                        else:
                            found = (
                                _expression_ended(
                                    previous, before_previous, postfix)
                                and starts_statement(token)
                                and _starts_line(token, line, last_line))
                    if found:
                        self.before_token = True
                was_busy = busy
                busy = found
                if was_busy:
                    head_started = head_starts
                    body_started = body_starts
                    label_started = label_starts
                    head_starts = body_starts = label_starts = False
                    loop_ended = False
                if tracked or was_busy:
                    if token in OPENERS:
                        if head_started and token == '(':
                            head = groups
                        groups += 1
                    elif token in CLOSERS:
                        if groups:
                            groups -= 1
                        if head == groups:
                            head = None
                            loop_ended = loop_ends
                            body_starts = not loop_ends
                            loop_ends = False
                            busy = True
                    elif token == '}':
                        context.closed_class_expression = False
                        object_ended = bool(blocks) and is_object
                        if blocks:
                            (groups, head, open_loops, loop_ends,
                             is_object) = blocks.pop()
                        if not (groups or object_ended):
                            last_closed_body = context.last_closed_function_body
                    elif token in ('{', '${'):
                        blocks.append(
                            (groups, head, open_loops, loop_ends, is_object))
                        is_object = _object_starts(
                            token, previous, before_previous, groups,
                            line == last_line)
                        # The braces of an object literal, a JSX child or
                        # a template interpolation hold an expression, as
                        # parentheses do.
                        groups = 1 if (
                            is_object or token == '${'
                            or isinstance(token, JSXChildBrace)) else 0
                        head = None
                        open_loops = 0
                        loop_ends = False
                    elif token in POSTFIX:
                        postfix = (
                            line == last_line
                            and _value_ended(
                                previous, before_previous, postfix)
                            and not (head_started or body_started))
                    elif tracked and previous not in MEMBER_ACCESS:
                        if token in HEADED:
                            head_starts = busy = True
                            # In `do while (a) run(); while (b)` the first
                            # "while" is the body.
                            if (token == 'while' and open_loops
                                    and not body_started):
                                open_loops -= 1
                                loop_ends = True
                        elif token in UNHEADED:
                            body_starts = busy = True
                            if token == 'do':
                                open_loops += 1
                        elif head_started:          # for await (…)
                            head_starts = busy = True
                    elif body_started:
                        label_starts = busy = True
                    elif label_started and token == ':':
                        # `do retry: while (…) …`
                        body_starts = busy = True
                    if was_busy:
                        # These are False between the tokens.
                        head_started = body_started = label_started = False
                last_line = line
            before_previous = previous
            previous = token
            yield token


def _starts_line(token, line, last_line):
    """Whether the token starts on a later line than the last one ended,
    and is not the '{' of a JSX child."""
    return (line - token.count('\n') != last_line
            and not isinstance(token, JSXChildBrace))


def _function_closed(context, last_closed_body):
    """Whether the reader closed a function body while reading the brace."""
    function = context.last_closed_function_body
    return (function is not last_closed_body
            and not getattr(function, 'is_braceless_declaration', False))


def _expression_ended(previous, before_previous, postfix):
    if before_previous in MEMBER_ACCESS:    # a property name: `token.in`
        return True
    if previous in _SPECIAL_ENDS:
        if previous in POSTFIX:
            return postfix
        if previous in ASSERTED_TYPES and before_previous in ASSERTIONS:
            return True
        if previous in UNFINISHED:
            return False
    return ends_expression(previous)


def _value_ended(previous, before_previous, postfix):
    """Whether an operator after the previous token is a postfix one."""
    if previous in POSTFIX:             # TypeScript: `item.owner!!`
        return postfix
    return before_previous in MEMBER_ACCESS or (
        previous != '}' and previous not in NOT_VALUES
        and ends_expression(previous))


def _object_starts(token, previous, before_previous, groups, same_line):
    if token != '{' or isinstance(token, JSXChildBrace):
        return False
    if previous == ':':
        # A property value or a branch of "?:", not a `case` or a label.
        return groups > 0
    if before_previous in MEMBER_ACCESS and previous not in OPENERS:
        return False                # a property name: `typeof Array.of {`
    if previous in VALUE_WORDS:
        return same_line                # not `return` + line break
    return opens_object(previous)
