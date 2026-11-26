"""Track the ends of Rust and Swift conditions for nesting depth."""
from lizard_languages.swift_conditions import _brace_is_body


_STARTS = frozenset(('if', 'while'))
_LOGICAL = frozenset(('&&', '||'))
_RUST_GROUP_PREFIX = _STARTS | _LOGICAL | {'(', '!', '='}
_RUST_VALUE_PREFIX = _STARTS | {'(', 'unsafe', 'const', '='}
_RUST_BLOCK_PREFIX = frozenset((
    'else', '&&', '||', '!', '=', '==', '!=',
    '<', '<=', '>', '>=', '+', '-', '*', '/', '%', '|', '&', '^',
    'unsafe', 'const', 'async', 'loop',
))
_EXPRESSION = 1
_SWIFT_TRAILING = 2
_RUST_VALUE_BLOCK = 3


class ConditionHeaders:  # pylint: disable=too-many-instance-attributes
    """A header counts one logical level and lends its levels to its braces."""

    @classmethod
    def for_reader(cls, reader):
        language = getattr(reader, 'language_names', ())
        if 'rust' in language or 'swift' in language:
            return cls(reader.context, swift='swift' in language)
        return None

    def __init__(self, context, swift):
        self._context = context
        self._swift = swift
        self._groups = 0
        self._heads = []
        self._braces = []
        self._closed = None
        self._previous = None
        self._rust_unary_not = False
        self._swift_logical = _SwiftGroupedLogic() if swift else None

    def advance(self, token):
        """Read a token and return True if its logical level is handled."""
        if token == '\n':
            return False
        repeat_tail = self._closed is not None and self._resume_condition(token)
        head = self._head()
        starts = token in _STARTS and not repeat_tail and not getattr(token, 'is_member_name', False)
        if self._swift_logical is not None:
            self._swift_logical.advance(self._context, token, self._groups, starts, head is not None)
        expression = self._rust_expression(token)
        handled = token in _LOGICAL and self._read_logical(head, expression)
        if not handled:
            self._advance_structure(token, head, starts)
        if token == '!' and not self._swift:
            self._rust_unary_not = self._previous in _RUST_GROUP_PREFIX or self._previous in _RUST_BLOCK_PREFIX
        self._previous = token
        return handled

    def _read_logical(self, head, expression):
        if head is not None:
            if not head.logical:
                self._context.open_level()
                head.logical = True
            return True
        if expression is None:
            return False
        if expression.role == _RUST_VALUE_BLOCK:
            if not expression.tail_logical:
                self._context.open_level()
                expression.tail_logical = True
            return True
        head = expression.heads[-1]
        if not head.logical:
            # The field belongs to the header that lent its levels to this brace.
            expression.owner.block_levels[-1] += 1
            expression.levels += 1
            self._context.add_nd_condition()
            head.logical = True
        return True

    def _rust_expression(self, token):
        if not self._swift and self._braces:
            brace = self._braces[-1]
            if isinstance(brace, _ConditionBrace) and brace.owner is self._context.current_function:
                if brace.role == _RUST_VALUE_BLOCK:
                    if token == ';' and self._groups == brace.groups:
                        brace.tail_logical = False
                    return brace
                brace.check_first_field(token, self._previous)
                if brace.has_fields and brace.owner.block_levels:
                    return brace
        return None

    def _advance_structure(self, token, head, starts):
        if token in ('(', '['):
            if token == '(' and not self._swift and head and self._previous in _RUST_GROUP_PREFIX:
                if head.is_rust_operand_group(self._groups):
                    head.operand_groups.add(self._groups + 1)
            self._groups += 1
        elif token in (')', ']'):
            if head and not self._swift:
                head.operand_groups.discard(self._groups)
            self._groups -= 1
            while self._heads and self._heads[-1].groups > self._groups:
                self._heads.pop()
        elif token == '{':
            self._open_brace(head)
        elif token == '}':
            self._close_brace()
        elif starts:
            self._heads.append(_Header(self._context.current_function, self._groups))
        elif head and self._groups == head.groups:
            if token in (';', '=>'):
                self._heads.pop()
            elif not self._swift:
                head.read_rust_token(token)

    def _head(self):
        if self._heads and self._heads[-1].owner is self._context.current_function:
            return self._heads[-1]
        return None

    def _open_brace(self, head):
        if head is None:
            self._braces.append(self._swift and self._previous == 'repeat'
                                and not getattr(self._previous, 'is_member_name', False))
            return
        role = self._brace_role(head)
        self._braces.append(_ConditionBrace(
            self._heads, role, self._groups, rust_fields=not self._swift and self._groups > head.groups))
        self._heads = []
        self._context.reset_condition_tracking()

    def _brace_role(self, head):
        if not self._swift and head.is_rust_operand_group(self._groups) and (
                self._previous in _RUST_VALUE_PREFIX or self._previous == '!' and self._rust_unary_not):
            return _RUST_VALUE_BLOCK
        if self._groups > head.groups:
            return _EXPRESSION
        if self._swift:
            return _SWIFT_TRAILING
        if head.pattern or head.match or self._previous in _RUST_BLOCK_PREFIX:
            head.match = False
            return _EXPRESSION
        return 0

    def _close_brace(self):
        if not self._braces:
            return
        self._closed = self._braces.pop()
        if isinstance(self._closed, _ConditionBrace) and self._closed.owner is self._context.current_function:
            # Any pending levels from inside this brace end here.
            self._closed.close(self._context.current_line)
            self._context.end_statement()

    def _resume_condition(self, token):
        brace, self._closed = self._closed, None
        if not isinstance(brace, _ConditionBrace):
            return brace is True and token == 'while'
        if brace.owner is not self._context.current_function:
            return False
        function = brace.owner
        function.condition_depth, function.in_condition, function.logical_operator_added = brace.condition
        expression = brace.role in (_EXPRESSION, _RUST_VALUE_BLOCK) or (
            brace.role == _SWIFT_TRAILING and brace.closing_line == self._context.current_line
            and not _brace_is_body('if', token))
        self._heads = brace.heads
        levels = brace.levels
        if not expression:
            finished = self._heads.pop()
            levels = finished.outer_levels if self._heads else 0
        if levels:
            function.levels_awaiting_body += levels
            self._context.add_nd_condition(levels)
        return False


class _SwiftGroupedLogic:  # pylint: disable=too-few-public-methods
    """Keep a completed grouped expression's logical levels out of the next header."""

    def __init__(self):
        self._owner = None
        self._before_operator = None
        self._levels = self._groups = 0

    def advance(self, context, token, groups, starts, in_header):  # pylint: disable=too-many-arguments
        function = context.current_function
        if function is not self._owner or token in (';', '{', '}'):
            self._owner = function
            self._before_operator = None
            self._levels = self._groups = 0
        if self._before_operator is not None:
            self._levels += function.levels_awaiting_body - self._before_operator
            self._before_operator = None
        if starts and self._levels and groups < self._groups:
            function.levels_awaiting_body -= self._levels
            context.add_nd_condition(-self._levels)
            self._levels = 0
        elif groups and token in _LOGICAL and not in_header:
            self._groups = min(self._groups, groups) if self._levels else groups
            self._before_operator = function.levels_awaiting_body


class _Header:

    def __init__(self, owner, groups):
        self.owner = owner
        self.groups = groups
        self.outer_levels = owner.levels_awaiting_body
        self.logical = False
        self.pattern = False
        self.match = False
        self.operand_groups = set()

    def is_rust_operand_group(self, depth):
        return depth == self.groups or depth in self.operand_groups

    def read_rust_token(self, token):
        if token == 'let':
            self.pattern = True
        elif token == '=':
            self.pattern = False
        elif token == 'match':
            self.match = True


class _ConditionBrace:  # pylint: disable=too-many-instance-attributes

    def __init__(self, heads, role, groups, rust_fields):
        self.heads = heads
        self.role = role
        self.groups = groups
        self.owner = owner = heads[-1].owner
        self.levels = owner.levels_awaiting_body
        self.closing_line = None
        self.condition = (owner.condition_depth, owner.in_condition, owner.logical_operator_added)
        self.has_fields = None if rust_fields else False
        self.tail_logical = False

    def close(self, line):
        self.closing_line = line
        head = self.heads[-1]
        if self.tail_logical and not head.logical:
            # At the closing brace, the final expression becomes the condition.
            head.logical = True
            self.levels += 1

    def check_first_field(self, token, previous):
        # A leading named field distinguishes a literal from a closure body.
        if self.has_fields is None and (previous != '{' or not token.isidentifier()):
            self.has_fields = previous != '{' and token == ':'
