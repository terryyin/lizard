'''
Language parser for Groovy, including Gradle scripts and Jenkinsfiles.

Methods and constructors are functions, and so is a closure assigned to a
field or a script variable (`def retry = { n -> ... }`). Other closures
belong to the method around them. A call with a trailing closure, such as
`stage('Build') { ... }`, is not a function.
'''

import os
import re

from .code_reader import CodeReader, CodeStateMachine
from .token_pattern import compiled_token_pattern


class _MemberName(str):
    """A keyword used as a name, `x.class` or `[if: 1]`: no metric counts it."""

    is_member_name = True


class _InString(str):
    """An operator in the `${...}` of a string: no table row reads it."""
    in_string = True


_KEYWORDS = frozenset((
    'if', 'else', 'for', 'while', 'do', 'switch', 'case', 'default', 'try',
    'catch', 'finally', 'class', 'interface', 'enum', 'trait', 'record',
    'new', 'def', 'in', 'return', 'break', 'continue', 'throw', 'assert'))
_MEMBER_ACCESS = frozenset(('.', '?.', '*.', '.&', '.@', '?.@', '::'))
# After these a `/` divides; elsewhere it starts a slashy string.
_DIVIDEND_ENDS = frozenset((')', ']', '}', '++', '--'))
_REGEX_AFTER_WORDS = frozenset((
    'return', 'case', 'in', 'assert', 'throw', 'else', 'yield', 'when'))
# A line comment ends at the newline, even after a backslash.
_GROOVY_TOKENS = (
    r"|//[^\n]*"
    r"|'''(?:\\.|[^\\])*?'''"
    r'|"""|"|\$/'
    r"|\w*\$[\w$]*"
    r"|\?\.@|\?\.|\?\[|\?:|\?=|\*\.|\.&|\.@")
_MAP_KEY_COLON = re.compile(r'[ \t]*:(?!:)')
# What ends the text of a string, or starts an interpolation in it.
_STRING_SPECIALS = {
    '"': re.compile(r'\\.|"|\n|\$\{', re.S),
    '"""': re.compile(r'\\.|"""|\$\{', re.S),
    '/': re.compile(r'\\/|/|\$\{'),
    '$/': re.compile(r'\$\$|\$/|/\$|\$\{'),
}
_STRING_ENDS = {'"': '"', '"""': '"""', '/': '/', '$/': '/$'}
# Groovy reads a `/` as division when no slashy string can close after it.
_SLASHY_CLOSES = re.compile(r'(?:[^/\\]|\\/|\\(?!/))*/')
_STRING_OPENERS = frozenset(('"', '"""', '$/'))


def _starts_slashy_string(previous):
    if previous is None:
        return True
    if previous in _DIVIDEND_ENDS or previous[0] in '\'"':
        return False
    if previous[0].isalnum() or previous[0] in '_$':
        return previous in _REGEX_AFTER_WORDS
    return True


def _code_tokens(pattern, source, pos, in_interpolation=False):
    """Yield code tokens from pos; return the end of an interpolation."""
    previous = None
    brackets = []
    while True:
        for match in pattern.finditer(source, pos):
            token = match.group()
            if token in _STRING_OPENERS or (
                    token in ('/', '/=') and _starts_slashy_string(previous)
                    and _SLASHY_CLOSES.match(source, match.start() + 1)):
                pos = yield from _string_tokens(
                    pattern, source, match.start(), token[:1] if token == '/=' else token)
                previous = '""'
                break
            if token in ('(', '[', '?[', '{'):
                brackets.append(token[-1])
            elif token in (')', ']', '}'):
                if not brackets and in_interpolation:
                    return match.end()
                if brackets:
                    brackets.pop()
            if in_interpolation and token in ('||', '=', '<<', ';'):
                token = _InString(token)
            if token in _KEYWORDS and (
                    previous in _MEMBER_ACCESS
                    or _MAP_KEY_COLON.match(source, match.end())):
                token = _MemberName(token)
            if token == '\n' and (not brackets or brackets[-1] == '{'):
                previous = None  # A new statement: `/` starts a string.
            elif not token.isspace() and not token.startswith(('//', '/*')):
                previous = token
            yield token
        else:
            return len(source)


def _string_tokens(pattern, source, start, opener):
    """Yield a string literal as text tokens around the code tokens of its
    `${...}` interpolations; return where it ends."""
    specials, closer = _STRING_SPECIALS[opener], _STRING_ENDS[opener]
    pos = text = start + len(opener)
    while True:
        match = specials.search(source, pos)
        if match is None or match.group() == '\n':
            end = len(source) if match is None else match.start()
            yield '"' + source[text:end] + '"'
            return end
        pos = match.end()
        if match.group() == closer:
            yield '"' + source[text:match.start()] + '"'
            return pos
        if match.group() == '${':
            yield '"' + source[text:match.start()] + '"'
            pos = text = yield from _code_tokens(pattern, source, pos, True)


# Outside a `where:` block only these tokens can change the table state.
_TABLE_TOKENS = frozenset(('\n', '{', '(', '[', '?[', '}', ')', ']', ':'))


class _DataTables:  # pylint: disable=R0902,R0903
    """Turns each `||` that separates the columns of a row in a Spock data
    table into `|`, so that no metric counts it. A `where:` block ends at
    its closing brace or at a `filter:` block. A data pipe (`<<`) or a
    variable (`=`) is no row. A row with two statements has `;` columns,
    so its `||` count. The tokens of a row wait in a buffer until the row
    ends, at a line end that ends its statement."""

    def __init__(self):
        self.brackets, self.where = [], None
        self.row, self.line_ends = False, True
        self.recent = ('\n', '\n')
        self.line, self.columns = [], []
        self.statements, self.in_statement = 0, False

    def __call__(self, tokens):
        line = self.line
        for token in tokens:
            if token.isspace() and token != '\n':
                continue
            if self.where is None and not line:
                if token in _TABLE_TOKENS:
                    self._outside_tables(token)
                if token[0] != '/' or token[:2] not in ('//', '/*'):
                    self.recent = (self.recent[1], token)
                yield token
                continue
            self.line_ends = False
            self._read(token, len(line))
            if token[0] != '/' or token[:2] not in ('//', '/*'):
                if token != '\n' or self.line_ends:
                    self.recent = (self.recent[1], token)
            line.append(token)
            if self.line_ends or self.where is None:
                yield from self._end_line()

    def _outside_tables(self, token):
        brackets = self.brackets
        if token in ('{', '(', '[', '?['):
            brackets.append(token[-1])
        elif token in ('}', ')', ']'):
            if brackets:
                brackets.pop()
        elif (token == ':' and self.recent[1] == 'where' and self.recent[0] == '\n'
              and brackets[-1:] == ['{']):
            self._label('where')

    def _end_line(self):
        line = self.line
        if self.statements + self.in_statement < 2:
            for index in self.columns:
                line[index] = '|'
        yield from line
        del line[:], self.columns[:]
        self.statements, self.in_statement = 0, False

    def _read(self, token, index):
        brackets = self.brackets
        if token == '\n' or token[:2] == '/*' and '\n' in token:
            self._new_line()
            return
        if token.startswith(('//', '/*')):
            return
        if token in ('{', '(', '[', '?['):
            brackets.append(token[-1])
        elif token in ('}', ')', ']'):
            if brackets:
                brackets.pop()
            if self.where is not None and len(brackets) < self.where:
                self.where = None
                return
        elif (token == ':' and self.recent[0] == '\n' and brackets[-1:] == ['{']
              and self.where in (None, len(brackets))):
            self._label(self.recent[1])
            return
        elif self.where == len(brackets):
            self._row_token(token, index)
            return
        self.in_statement = True

    def _new_line(self):
        """A line end ends a statement unless an operator continues it."""
        previous = self.recent[1]
        if previous in ('\n', ':', ';', '{', '}') or _ends_expression(previous):
            self.row = self.line_ends = True

    def _row_token(self, token, index):
        if getattr(token, 'in_string', False):
            return
        if token == ';':
            self.statements += self.in_statement
            self.in_statement = False
            return
        self.in_statement = True
        if token in ('=', '<<') or token in ('||', '&&') and self.recent[1] == '\n':
            self.row = False
        elif token == '||' and self.row:
            self.columns.append(index)

    def _label(self, label):
        if label == 'where':
            self.where, self.row = len(self.brackets), True
        elif label == 'filter':
            self.where = None


_HEADED = frozenset(('if', 'for', 'while', 'switch', 'catch'))
_UNHEADED = frozenset(('else', 'do', 'try', 'finally'))
_INFIX_WORDS = frozenset(('in', 'instanceof', 'as'))


def _ends_expression(token):
    if _is_word(token) or token[0].isdigit() or token[0] in '\'"':
        return token not in _INFIX_WORDS and token != 'new'
    return token in (')', ']', '}', '++', '--')


def _starts_statement(token):
    if _is_word(token) or token[0].isdigit() or token[0] in '\'"':
        return token not in _INFIX_WORDS
    return token in ('@', '{', '}', '(', '[', '!', '++', '--')


class _StatementEnds:  # pylint: disable=R0903
    """Where a statement ends without a `;`, for the nesting extensions.

    A line break ends it, unless it is inside parentheses or brackets, a
    structure waits for its body (`if (ready)` + line break + `run()`), or
    the tokens around it continue the expression. So does a `}`.

    read(tokens) passes each token on, in order.  While a token is passed
    on, before_token tells whether a statement ends before it."""

    def __init__(self, context):
        self.before_token = False
        self._context = context
        self._line = 1
        self._previous = '{'
        self._groups = []       # open '(', '[' and '{'; 'head' for `if (`
        self._head = False      # the next '(' opens the head of a structure
        self._body = False      # a structure waits for its body

    def read(self, tokens):
        for token in tokens:
            self.before_token = self._ends_before(token)
            yield token

    def _ends_before(self, token):
        line = self._context.current_line - token.count('\n')
        ends = ((token == '}' or line != self._line)
                and not self._body and self._previous != '}'
                and (not self._groups or self._groups[-1] == '{')
                and _ends_expression(self._previous)
                and _starts_statement(token))
        self._read(token)
        self._previous = token
        self._line = self._context.current_line
        return ends

    def _read(self, token):
        self._body = False
        if _is_member_name(token):
            self._head = False
            return
        if token in ('(', '[', '?[', '{'):
            self._groups.append(
                'head' if self._head and token == '(' else token[-1])
        elif token in (')', ']', '}'):
            self._body = bool(self._groups) and self._groups.pop() == 'head'
        elif token in _UNHEADED:
            self._body = True
        self._head = token in _HEADED


class GroovyReader(CodeReader):
    # pylint: disable=R0903

    ext = ['groovy', 'gvy', 'gy', 'gsh', 'gradle']
    language_names = ['groovy']

    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    # The ternary, the Elvis operator and the Elvis assignment.
    _ternary_operators = {'?', '?:', '?='}
    # Nesting depth: the default without `def`, which declares here.
    loops = frozenset((
        'if', 'for', 'while', 'catch', 'case', 'try', '&&', '||', '?', '?:'))

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [GroovyStates(context)]
        self._statement_ends = None

    def implied_semicolons(self, tokens):
        """For the nesting extensions, as in the TypeScript reader: the
        tokens, and the finder of the statement ends without a `;`.  The
        first extension that asks starts the finder."""
        if self._statement_ends is None:
            self._statement_ends = _StatementEnds(self.context)
            tokens = self._statement_ends.read(tokens)
        return tokens, self._statement_ends

    @classmethod
    def match_filename(cls, filename):
        return (os.path.basename(filename) == 'Jenkinsfile'
                or super().match_filename(filename))

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith(('/*', '//', '#!')):
            return token[2:]
        return None

    @staticmethod
    def preprocess(tokens):
        return _DataTables()(tokens)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        pattern = compiled_token_pattern(_GROOVY_TOKENS + addition)
        pos = 0
        if source_code.startswith('#!'):
            pos = source_code.find('\n')
            pos = len(source_code) if pos < 0 else pos
            yield source_code[:pos]
        yield from _code_tokens(pattern, source_code, pos)


_DECLARATION, _BODY = 'declaration', 'body'
_MODIFIERS = frozenset((
    'def', 'void', 'boolean', 'byte', 'char', 'short', 'int', 'long', 'float',
    'double', 'public', 'private', 'protected', 'static', 'final', 'abstract',
    'synchronized', 'native', 'transient', 'volatile', 'strictfp', 'default',
    'sealed'))
_TYPE_KEYWORDS = frozenset(('class', 'interface', 'enum', 'trait', 'record'))
_CONTEXTUAL_TYPE_KEYWORDS = frozenset(('trait', 'record'))
_TYPE_HEADER = frozenset((
    '.', ',', '<', '>', '?', '&', '[', ']', '(', ')', '@'))
_CLOSURE_PARAMETERS = frozenset((',', '.', '<', '>', '?', '...'))
_STATEMENT_WORDS = _KEYWORDS - _MODIFIERS - _CONTEXTUAL_TYPE_KEYWORDS
_CLOSERS = {')': '(', ']': '[', '}': '{', '>': '<'}


def _is_word(token):
    return token[0].isalpha() or token[0] in '_$'


def _is_member_name(token):
    return getattr(token, 'is_member_name', False)


def _is_type_name(token):
    return _is_word(token) and token[0].isupper()


def _starts_type(token):
    return _is_type_name(token) or token == '?' or _is_type_arguments(token)


def _is_type_arguments(token):
    """The token pattern reads simple type arguments as one token: `<?>`."""
    return len(token) > 1 and token[0] == '<' and token[-1] == '>' and token != '<=>'


def _is_name(token):
    return token is not None and (
        (_is_word(token) and token not in _STATEMENT_WORDS
         and not _is_member_name(token))
        or token[0] in '\'"')


def _is_declaration_prefix(token):
    """`def`, a modifier or a type before a method name."""
    return token is not None and (
        token in _MODIFIERS or token[0].isupper() or token[-1] in '>]')


class _Scope:  # pylint: disable=R0903
    """An open brace: a script or class body declares methods, others not."""

    def __init__(self, kind, class_name=None, is_enum=False):
        self.kind = kind
        self.class_name = class_name
        self.is_enum = is_enum
        self.parens = 0


class GroovyStates(CodeStateMachine):  # pylint: disable=R0902
    """Finds methods and closure fields in the scopes that declare them:
    the script and class bodies. Each `{` opens a scope and a nesting."""

    def __init__(self, context):
        super().__init__(context)
        self.scopes = [_Scope(_DECLARATION)]
        self.recent = [None, None, None]     # the last three tokens
        self.brackets = []
        self.closure_parameters, self.closure_brackets = [], []
        self.closure_default = self.type_arguments = False
        self.type_name = None
        self.type_is_enum = False
        self.new_parens = []
        self.after_new_arguments = False

    def _remember(self, token):
        self.recent = [token, self.recent[0], self.recent[1]]

    def _state_global(self, token):
        if self.context.newline and self.recent[0] != '=':
            self._remember('\n')  # An `=` continues on the next line.
        scope = self.scopes[-1]
        after_new_arguments = self.after_new_arguments
        self.after_new_arguments = False
        if token in ('(', '['):
            if token == '(' and self._starts_method(scope):
                self._start_parameters()
                return
            scope.parens += 1
        elif token in (')', ']'):
            scope.parens = max(scope.parens - 1, 0)
            if self.new_parens and self.new_parens[-1] == (scope, scope.parens):
                self.new_parens.pop()
                self.after_new_arguments = True
        elif token == '{':
            self._open_scope(scope, after_new_arguments)
        elif token == '}':
            self._close_scope()
        elif _is_member_name(token):
            pass
        elif token == 'new':
            self._state = self._new_type
        elif token in _TYPE_KEYWORDS:
            self.type_is_enum = token == 'enum'
            self._state = self._type_name
        self._remember(token)

    def _starts_method(self, scope):
        if scope.kind != _DECLARATION or scope.parens:
            return False
        name, prefix = self.recent[0], self.recent[1]
        # A class body holds no calls, so `name(` at a line start declares
        # a method (`@Autowired` on the line above, no type).
        return _is_name(name) and (
            _is_declaration_prefix(prefix) or name == scope.class_name
            or prefix == '\n' and scope.class_name and not scope.is_enum)

    def _start_parameters(self):
        name = self.recent[0]
        class_name = next(
            (s.class_name for s in reversed(self.scopes) if s.class_name), None)
        self.context.stacked_functions.append(self.context.current_function)
        self.context.try_new_function(name)
        function = self.context.current_function
        function.name = function.long_name = (
            class_name + '::' + name if class_name else name)
        function.defined_in_function = (
            len(self.context.stacked_functions) > 1)
        self.brackets = ['(']
        self._state = self._parameters

    def _settle_type_arguments(self, brackets, token):
        """A `<` after a type name opens type arguments if a type follows.
        In a default value, `N < 2` compares."""
        if self.type_arguments:
            self.type_arguments = False
            if not _starts_type(token):
                brackets.pop()

    def _parameters(self, token):
        self._settle_type_arguments(self.brackets, token)
        if token == '<' and not _is_type_name(self.last_token):
            pass  # A comparison in a default value.
        elif token in ('(', '[', '?[', '{', '<'):
            self.brackets.append(token[-1])
            self.type_arguments = token == '<'
            if token == '{':
                self.context.add_bare_nesting()
        elif token in _CLOSERS:
            if token == '}':
                self.context.pop_nesting()
            opener = _CLOSERS[token]
            if token == '>' and self.brackets[-1] != '<':
                pass
            else:
                while self.brackets and self.brackets.pop() != opener:
                    pass
                if not self.brackets:
                    self._state = self._after_parameters
                return
        elif token == '?' and self.brackets[-1] == '<':
            self.context.add_condition(-1)  # a wildcard type argument
        if len(self.brackets) == 1:
            self.context.parameter(token)

    def _after_parameters(self, token):
        if token == '{':
            decisions = self.context.current_function.cyclomatic_complexity
            self.context.confirm_new_function()
            # Decisions in default values count for the method.
            self.context.current_function.cyclomatic_complexity = decisions
            self.context.add_bare_nesting()
            self.scopes.append(_Scope(_BODY))
            self._state = self._state_global
            self._remember(token)
        elif token == 'throws':
            self._state = self._throws
        else:
            self.fold_into_enclosing_function()
            self._state = self._state_global
            self._remember(')')
            self._state_global(token)

    def _throws(self, token):
        if token == '{' or not (_is_word(token) or token in ',.<>') or (
                self.context.newline and self.last_token not in (',', '.', 'throws')):
            self._state = self._after_parameters
            self._after_parameters(token)

    def _open_scope(self, scope, after_new_arguments):
        if after_new_arguments or self._starts_enum_constant_body(scope):
            self.scopes.append(_Scope(_DECLARATION, '(anonymous)'))
        elif self._starts_closure_field(scope):
            self._start_closure(self.recent[1])
            return
        else:
            self.scopes.append(_Scope(_BODY))
        self.context.add_bare_nesting()
        self._remember('{')

    def _starts_enum_constant_body(self, scope):
        """`RED(1) {` or `BLUE {` in an enum body."""
        previous = self.recent[0]
        return scope.is_enum and scope.parens == 0 and (
            previous == ')' or _is_name(previous) and previous not in _MODIFIERS)

    def _starts_closure_field(self, scope):
        """`name = {` in a class or script body."""
        before, name, assign = self.recent[2], self.recent[1], self.recent[0]
        if scope.kind != _DECLARATION or scope.parens or assign != '=':
            return False
        return (_is_name(name) and name[0] not in '\'"'
                and before not in _MEMBER_ACCESS)

    def _start_closure(self, name):
        class_name = next(
            (s.class_name for s in reversed(self.scopes) if s.class_name), None)
        self.context.push_new_function(
            class_name + '::' + name if class_name else name)
        self.context.add_bare_nesting()
        self.scopes.append(_Scope(_BODY))
        self.closure_parameters, self.closure_brackets = [], []
        self.closure_default = False
        self._state = self._closure_parameters
        self._remember('{')

    def _close_scope(self):
        if len(self.scopes) > 1:
            self.scopes.pop()
        self.context.pop_nesting()

    def _closure_parameters(self, token):
        """Collect `{ a, List<T> b = [1, 2] ->` and replay the tokens as
        code when the closure has no parameter list."""
        pending, brackets = self.closure_parameters, self.closure_brackets
        self._settle_type_arguments(brackets, token)
        if token == '->' and not brackets:
            for parameter, visible in pending:
                if visible:
                    self.context.parameter(parameter)
                elif parameter == '?' and visible is None:
                    self.context.add_condition(-1)  # a wildcard type argument
            self._state = self._state_global
            self._remember(token)
            return
        hidden = bool(brackets)
        if token in ('(', '[', '?[') or (token == '{' and self.closure_default) or (
                token == '<' and _is_type_name(self.last_token)):
            brackets.append(token[-1])
            self.type_arguments = token == '<'
        elif token in (')', ']', '}') or (token == '>' and brackets[-1:] == ['<']):
            if brackets[-1:] != [_CLOSERS[token]]:
                return self._replay_closure_parameters(token)
            brackets.pop()
        elif token == ',' and not brackets:
            self.closure_default = False
        elif token == '=' and not brackets:
            self.closure_default = True
        elif token == ';' and not brackets or not (
                self.closure_default or token in _CLOSURE_PARAMETERS
                or _is_type_arguments(token)
                or _is_word(token) and token not in _KEYWORDS):
            return self._replay_closure_parameters(token)
        wildcard = token == '?' and brackets[-1:] == ['<']
        pending.append((token, None if wildcard else not (hidden or brackets)))
        return None

    def _replay_closure_parameters(self, token):
        self._state = self._state_global
        for pending, _ in self.closure_parameters:
            self._state(pending)
        self._state(token)

    def _new_type(self, token):
        if token == '(':
            scope = self.scopes[-1]
            self.new_parens.append((scope, scope.parens))
        elif token == '[':
            self._state = self._new_array_type
            return
        elif _is_word(token) or token in ('.', '<', '>', ',', '?') \
                or token.startswith('<'):
            return
        self._state = self._state_global
        self._state_global(token)

    def _new_array_type(self, token):
        """`new Supplier<String[]>() {...}`, but not `new int[3]`."""
        if token == ']':
            self._state = self._new_type
        else:
            self._state = self._state_global
            self._state_global('[')
            self._state(token)

    def _type_name(self, token):
        if _is_word(token) and (token[0].isupper() or self.recent[0] not in
                                _CONTEXTUAL_TYPE_KEYWORDS):
            self.type_name = token
            self._state = self._type_header
            self._remember(token)
        else:
            self._state = self._state_global
            self._state_global(token)

    def _type_header(self, token):
        if token == '{':
            self.scopes.append(
                _Scope(_DECLARATION, self.type_name, self.type_is_enum))
            self.context.add_bare_nesting()
            self._state = self._state_global
            self._remember(token)
        elif not (_is_word(token) or token in _TYPE_HEADER
                  or token.startswith('<')):
            self._state = self._state_global
            self._state_global(token)
