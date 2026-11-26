'''
Language parser for C Sharp
'''

import re

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates, clike_token_pattern, clike_tokens
from .token_pattern import tokens_in_span


# The reserved words of C#, and `record`, which this reader also reads as
# a type keyword. Only these need `@` to be a name.
_KEYWORDS = (
    'abstract', 'as', 'base', 'bool', 'break', 'byte', 'case', 'catch',
    'char', 'checked', 'class', 'const', 'continue', 'decimal', 'default',
    'delegate', 'do', 'double', 'else', 'enum', 'event', 'explicit',
    'extern', 'false', 'finally', 'fixed', 'float', 'for', 'foreach', 'goto',
    'if', 'implicit', 'in', 'int', 'interface', 'internal', 'is', 'lock',
    'long', 'namespace', 'new', 'null', 'object', 'operator', 'out',
    'override', 'params', 'private', 'protected', 'public', 'readonly',
    'record', 'ref', 'return', 'sbyte', 'sealed', 'short', 'sizeof',
    'stackalloc', 'static', 'string', 'struct', 'switch', 'this', 'throw',
    'true', 'try', 'typeof', 'uint', 'ulong', 'unchecked', 'unsafe',
    'ushort', 'using', 'virtual', 'void', 'volatile', 'while')
_VERBATIM_KEYWORD = r"|@(?:" + "|".join(_KEYWORDS) + r")\b"
_NON_LF_LINE_BREAK = re.compile(r'\r(?!\n)|[\x85\u2028\u2029]')


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?', '??'}  # C# has both ?: and ?? (null-coalescing)

    def __init__(self, context):
        super(CSharpReader, self).__init__(context)
        self.parallel_states = [
            CSharpStates(context),
            CLikeNestingStackStates(context)
        ]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Both the literal scanner and the metric counters use LF for line breaks.
        if not source_code.isascii() or '\r' in source_code:
            source_code = _NON_LF_LINE_BREAK.sub('\n', source_code)
        # A verbatim identifier, `@if`, is one name and never a keyword.
        # // comments end on the same line (not C/C++ line continuation).
        patterns = r"|(?:\?\?)" + _VERBATIM_KEYWORD
        if any(prefix in source_code for prefix in ('$"', '@"', '"""')):
            tokens = _literal_tokens(source_code, patterns, token_class)
        else:
            tokens = clike_tokens(source_code, patterns, token_class)
        if token_class is None and '?' in source_code:
            tokens = _mark_questions(tokens)
        return tokens

    def __call__(self, tokens, reader):
        # Interpolation code belongs to metrics, but cannot start or end
        # a function or class in the surrounding source.
        for token in tokens:
            if token == '?' and getattr(token, 'is_optional', False):
                self.context.add_condition(-1)
            if not isinstance(token, _InterpolationPart):
                for state in self.parallel_states:
                    state(token)
            yield token
        for state in self.parallel_states:
            state.statemachine_before_return()
        self.eof()


def _name(token):
    """The name in a verbatim identifier: `@if` names `if`."""
    if token[0] == '@' and token[1:2].isidentifier():
        return token[1:]
    return token


class CSharpStates(CLikeStates):
    def __init__(self, context):
        super(CSharpStates, self).__init__(context)
        self.in_primary_constructor = False
        self.class_name = None
        # (name to restore, brace depth when the type was declared). The
        # type's own "{" is not counted; "{" seen in the global state is.
        self._enclosing_classes = []
        self._open_braces = 0

    def try_new_function(self, name):
        if not self.in_primary_constructor:
            super(CSharpStates, self).try_new_function(name)
            if self.class_name and self.context.current_function:
                self.context.current_function.name = f"{self.class_name}::{name}"

    def _state_dec_to_imp(self, token):
        """Override to handle C# expression-bodied members (=>)"""
        if token == '=>':
            # Expression-bodied member: confirm function and enter expression body
            self.context.confirm_new_function()
            self._state = self._state_expression_body
        else:
            super(CSharpStates, self)._state_dec_to_imp(token)

    def _state_expression_body(self, token):
        """Read the expression body until semicolon, processing tokens for complexity"""
        if token == ';':
            # End of expression-bodied method, finalize function and return to global state
            self.context.end_of_function()
            self._state = self._state_global

    def _state_global(self, token):
        if token == '{':
            self._open_braces += 1
        elif token == '}':
            entered_at = (
                self._enclosing_classes[-1][1]
                if self._enclosing_classes else None)
            if entered_at is not None and self._open_braces == entered_at:
                self.class_name = self._enclosing_classes.pop()[0]
            elif self._open_braces > 0:
                self._open_braces -= 1
        if token in ("class", "struct", "record"):
            self._enclosing_classes.append(
                (self.class_name, self._open_braces))
            self.class_name = None
            self._state = self._state_class_declaration
        else:
            super(CSharpStates, self)._state_global(_name(token))

    def _state_class_declaration(self, token):
        if token == '(':  # Primary constructor
            self.in_primary_constructor = True
            self._state = self._state_primary_constructor
        elif token == '{':
            self._state = self._state_global
        elif _name(token)[0].isalpha():
            if not self.class_name:  # Only set class name if not already set
                self.class_name = _name(token)

    @CLikeStates.read_inside_brackets_then("()", "_state_class_declaration")
    def _state_primary_constructor(self, _):
        """Skip primary constructor parameters without counting them as a function"""
        self.in_primary_constructor = False


_OPEN_LITERAL = r'(?P<literal>\$*"{3,}|\$@"|@\$?"|\$")'
_CODE_UNITS = (
    r'(?m:^[^\S\n]*\#[^\n]*)|//[^\n]*|/\*.*?\*/|'
    + _OPEN_LITERAL
    + r'|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\''
)
_LITERAL = re.compile(_CODE_UNITS, re.S)
_HOLE = re.compile(_CODE_UNITS + r'|::|[()\[\]{}:]', re.S)
_REGULAR_TEXT = re.compile(r'\\.|\{\{|\}\}|["{]', re.S)
_VERBATIM_TEXT = re.compile(r'""|\{\{|\}\}|["{]')
_RAW_TEXT = re.compile(r'"+|\{+')
_FORMAT_END = re.compile(r'\\.|}', re.S)
_WHOLE_TOKEN = re.compile(r'.*', re.S)


class _InterpolationPart(str):
    """Part of a literal after its first token, visible only to metrics."""


def _literal_tokens(source, addition, token_class):
    """Tokenize literals and ordinary code at their original source positions."""
    pattern = clike_token_pattern(addition)
    pos = 0
    while True:
        opening = _LITERAL.search(source, pos)
        while opening and opening.group('literal') is None:
            opening = _LITERAL.search(source, opening.end())
        if opening is None:
            break
        start = opening.start()
        yield from tokens_in_span(pattern, source, pos, start, token_class)
        parts = []
        end = _read_literal(source, opening, parts)
        if end is None:
            end = opening.end()
            yield from tokens_in_span(pattern, source, start, end, token_class)
        else:
            yield from _emit_literal(source, pattern, opening, end, parts, token_class)
        pos = end
    yield from tokens_in_span(pattern, source, pos, len(source), token_class)


def _emit_literal(source, pattern, opening, end, parts, token_class):
    start = opening.start()
    # Keep the separate dollar token for regular interpolation.
    if opening.group() == '$"':
        yield from tokens_in_span(pattern, source, start, start + 1, token_class)
        start += 1
    if token_class is not None:
        yield token_class(_WHOLE_TOKEN.match(source, start, end))
        return
    first = True
    for begin, stop in (*parts, (end, end)):
        if start < begin:
            fragment = source[start:begin]
            yield fragment if first else _InterpolationPart(fragment)
            first = False
        for token in tokens_in_span(pattern, source, begin, stop):
            yield _InterpolationPart(token)
        start = stop


def _read_literal(source, opening, parts):
    """Find a literal end and collect disjoint spans inside its holes."""
    opener = opening.group()
    prefix = opener.rstrip('"')
    quotes = len(opener) - len(prefix)
    dollars = prefix.count('$')
    verbatim = '@' in prefix
    if quotes >= 3:
        special = _RAW_TEXT
        open_braces = dollars
    else:
        special = _VERBATIM_TEXT if verbatim else _REGULAR_TEXT
        open_braces = 1 if dollars else 0
    pos = opening.end()
    while True:
        match = special.search(source, pos)
        if match is None:
            return None
        token = match.group()
        pos = match.end()
        if token[0] == '"' and len(token) == quotes:
            return pos
        if open_braces and token[0] == '{' and (
                len(token) >= open_braces if quotes >= 3 else token == '{'):
            pos = _read_hole(source, pos, open_braces, verbatim, quotes >= 3, parts)
            if pos is None:
                return None


def _read_hole(source, pos, braces, verbatim, raw, parts):
    start = pos
    depth = 0
    while True:
        match = _HOLE.search(source, pos)
        if match is None:
            return None
        token = match.group()
        pos = match.end()
        if match.group('literal') is not None:
            parts.append((start, match.start()))
            pos = _read_literal(source, match, parts)
            if pos is None:
                return None
            start = pos
        elif token in ('(', '[', '{'):
            depth += 1
        elif depth and token in (')', ']', '}'):
            depth -= 1
        elif not depth and token == ':':
            parts.append((start, match.start()))
            return _read_format(source, pos, braces, verbatim or raw)
        elif not depth and token == '}' and source.startswith('}' * braces, match.start()):
            parts.append((start, match.start()))
            return match.start() + braces


def _read_format(source, pos, braces, unescaped):
    close = '}' * braces
    if unescaped:
        end = source.find(close, pos)
        return end + braces if end != -1 else None
    for match in _FORMAT_END.finditer(source, pos):
        if match.group() == close:
            return match.end()
    return None


class _OptionalQuestion(str):
    """A nullable type or conditional access marker."""

    is_optional = True


class _OptionalHoleQuestion(_InterpolationPart):
    is_optional = True


_TYPE_WORDS = frozenset({
    'bool', 'byte', 'char', 'class', 'decimal', 'double', 'float',
    'int', 'long', 'object', 'sbyte', 'short', 'string', 'uint', 'ulong', 'ushort',
})
_DECLARATION_MODIFIERS = frozenset({
    'abstract', 'const', 'extern', 'internal', 'override', 'private', 'protected',
    'public', 'readonly', 'sealed', 'static', 'virtual', 'volatile',
})
_TYPE_END = frozenset(('(', '[', '{', ')', ']', '}', ';', '=', '=>'))
_TYPE_PATTERN_END = frozenset(('?', '??', '&&', '||', '=', '=>', ':', ';', ')', '{', '}'))
_NOT_A_TERNARY_ARM = frozenset(('.', ',', ':', ';', ')', ']', '}', '>', '='))
_OPEN_GROUP = frozenset(('(', '[', '{'))
_CLOSE_GROUP = frozenset((')', ']', '}'))


def _mark_questions(tokens):
    """Leave ternaries as ordinary '?' tokens for all metric consumers."""
    groups = [([], [])]
    buffered = []
    pending = 0
    question = None
    previous = before_previous = None
    reading_type = type_pattern = False
    type_brackets = constraint = 0
    for token in tokens:
        if buffered or token == '?':
            buffered.append(token)
        else:
            yield token

        if token.isspace() or token.startswith(('//', '/*', '#')):
            continue
        if buffered:
            if question is not None and token not in _NOT_A_TERNARY_ARM:
                # Before '[', the mark can be an index access or a ternary
                # with a collection expression. A direct ternary has priority.
                groups[-1][token == '['].append(question)
                pending += 1
            question = None
            if token == '?':
                index = len(buffered) - 1
                empty_rank = previous == ']' and before_previous in ('[', ',')
                if not reading_type and (type_pattern or not (empty_rank or previous in _TYPE_WORDS)):
                    question = index
                mark = _OptionalHoleQuestion if isinstance(token, _InterpolationPart) else _OptionalQuestion
                buffered[index] = mark(token)
            elif token == ':':
                # A named argument starts after the comma. Its colon does
                # not pair with a question mark in the previous argument.
                if constraint == 1 or (before_previous == ',' and previous.lstrip('@').isidentifier()):
                    pending -= _clear_questions(groups[-1])
                else:
                    pending -= _resolve_ternary(groups[-1], buffered)
            elif token in _OPEN_GROUP:
                groups.append(([], []))
            elif token in _CLOSE_GROUP:
                group = groups.pop() if len(groups) > 1 else groups[0]
                pending -= _clear_questions(group)
            elif token == ';':
                pending -= _clear_questions(groups[-1])
            if token == 'where' and previous == ')':
                constraint = 2
            elif constraint:
                constraint -= 1
            if question is None and not pending:
                yield from buffered
                buffered.clear()
                groups = [([], [])]

        if token == 'new' or token in _DECLARATION_MODIFIERS:
            reading_type = True
        elif reading_type:
            if token == '<' or (token == '(' and (type_brackets or previous in _DECLARATION_MODIFIERS)):
                type_brackets += 1
            elif token in ('>', ')') and type_brackets:
                type_brackets -= 1
            elif not type_brackets and token in _TYPE_END:
                reading_type = False
        if token == 'is':
            type_pattern = True
        elif type_pattern and token in _TYPE_PATTERN_END:
            type_pattern = False
        if token != '@':
            before_previous, previous = previous, token
    yield from buffered


def _clear_questions(group):
    direct, indexed = group
    count = len(direct) + len(indexed)
    direct.clear()
    indexed.clear()
    return count


def _resolve_ternary(group, buffered):
    direct, indexed = group
    candidates = direct or indexed
    if candidates:
        index = candidates.pop()
        mark = buffered[index]
        buffered[index] = _InterpolationPart(mark) if isinstance(mark, _InterpolationPart) else '?'
        return 1
    return 0
