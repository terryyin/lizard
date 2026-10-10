'''
Language parser for Apple Swift
'''

import re

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .golike import GoLikeStates

_LITERAL_START = re.compile(r'//|/\*|#*"')
_COMMENT_DELIMITER = re.compile(r'/\*|\*/')
_STRING_SPECIAL = re.compile(r'[\\"\n]')
_INTERPOLATION_SPECIAL = re.compile(r'[()\n]|#*"')


def _split_literals(source):
    """Yield (is_literal, text) pieces; literals are whole strings and
    block comments, which a regular expression cannot delimit because
    both nest."""
    start = pos = 0
    while True:
        match = _LITERAL_START.search(source, pos)
        if match is None:
            break
        if match.group() == '//':
            newline = source.find('\n', match.end())
            pos = len(source) if newline < 0 else newline
            continue
        if match.group() == '/*':
            end = _block_comment_end(source, match.start())
        else:
            end = _string_end(source, match.start(), len(match.group()) - 1)
        yield False, source[start:match.start()]
        yield True, source[match.start():end]
        start = pos = end
    yield False, source[start:]


def _block_comment_end(source, start):
    depth = 0
    for match in _COMMENT_DELIMITER.finditer(source, start):
        depth += 1 if match.group() == '/*' else -1
        if depth == 0:
            return match.end()
    return len(source)


def _string_end(source, start, hashes):
    quote = start + hashes
    multiline = source.startswith('"""', quote)
    close = ('"""' if multiline else '"') + '#' * hashes
    escape = '\\' + '#' * hashes
    pos = quote + (3 if multiline else 1)
    while True:
        match = _STRING_SPECIAL.search(source, pos)
        if match is None:
            return len(source)
        pos = match.start()
        if source.startswith(close, pos):
            return pos + len(close)
        if source.startswith(escape, pos):
            pos += len(escape)
            if source.startswith('(', pos):
                pos = _interpolation_end(source, pos, multiline)
            else:
                pos += 1
        elif source[pos] == '\n' and not multiline:
            return pos
        else:
            pos += 1


def _interpolation_end(source, start, multiline):
    depth = 0
    pos = start
    while True:
        match = _INTERPOLATION_SPECIAL.search(source, pos)
        if match is None:
            return len(source)
        token = match.group()
        pos = match.end()
        if token == '(':
            depth += 1
        elif token == ')':
            depth -= 1
            if depth == 0:
                return pos
        elif token == '\n':
            if not multiline:
                return match.start()
        else:
            pos = _string_end(source, match.start(), len(token) - 1)


class SwiftReplaceLabel:
    _DECLARATION_LABELS = []

    def preprocess(self, tokens):
        tokens = list(t for t in tokens if not t.isspace() or t == '\n')

        def replace_label(tokens, target, replace):
            for i in range(0, len(tokens) - len(target)):
                if tokens[i:i + len(target)] == target:
                    for j, repl in enumerate(replace):
                        tokens[i + j] = repl
            return tokens

        labels = [k for k in self.conditions if k.isalpha()]
        for k in labels + self._DECLARATION_LABELS:
            tokens = replace_label(tokens, ["(", k, ":"], ["(", "_" + k, ":"])
            tokens = replace_label(tokens, [",", k, ":"], [",", "_" + k, ":"])
        return tokens


class SwiftReader(CodeReader, CCppCommentsMixin, SwiftReplaceLabel):
    # pylint: disable=R0903

    FUNC_KEYWORD = 'def'
    ext = ['swift']
    language_names = ['swift']
    _DECLARATION_LABELS = ['init', 'subscript', 'deinit']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'guard'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}  # Pattern matching
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(SwiftReader, self).__init__(context)
        self.parallel_states = [SwiftStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = (
            r"|`\w+`" +
            r"|\w+\?" +
            r"|\w+\!" +
            r"|\?\?" +
            r"|\#(?!(?:if|elseif|else|endif|sourceLocation|warning|error)\b)\w+" +
            addition)
        for is_literal, text in _split_literals(source_code):
            if is_literal:
                yield text
            else:
                yield from CodeReader.generate_tokens(text, addition)


class SwiftStates(GoLikeStates):  # pylint: disable=R0903

    TYPE_KEYWORD = None

    # Failable initializers stay one token so `?` is not counted as a ternary.
    _FAILABLE_INITIALIZERS = {'init?': 'init', 'init!': 'init'}
    _ACCESSORS = {'get', 'set', 'willSet', 'didSet', 'deinit'}
    _ACCESSORS_WITH_PARAMETER = {'set', 'willSet', 'didSet'}
    _GETTER_EFFECTS = {'async', 'throws'}

    def __init__(self, context):
        super(SwiftStates, self).__init__(context)
        self._previous_token = None
        self._accessor = None
        self._accessor_step = None
        self._accessor_tokens = []

    def _state_global(self, token):
        name = self._introduced_function(token)
        if name is not None:
            self.context.push_new_function('')
            self.next(self._function_name, name)
        elif token in self._ACCESSORS and self._previous_token != '.':
            self._accessor = (token, self.context.current_line)
            self._accessor_step = 'name'
            self._accessor_tokens = []
            self._state = self._accessor_head
        elif token == 'protocol':
            self._state = self._protocol
        elif token in ('let', 'var', 'case', ','):
            self._state = self._expect_declaration_name
        else:
            super(SwiftStates, self)._state_global(token)
        if token != '\n':
            self._previous_token = token

    def _introduced_function(self, token):
        if self._previous_token == '.':
            return None
        if token in self._FAILABLE_INITIALIZERS:
            return self._FAILABLE_INITIALIZERS[token]
        if token in ('init', 'subscript'):
            return token
        return None

    def _accessor_head(self, token):
        # Expressions reuse the accessor words (`set.insert(x)`,
        # `Binding(get: ...)`, `private(set)`), so only a complete
        # accessor head followed by `{` starts a function.
        self._accessor_tokens.append(token)
        if token == '{' and self._accessor_step in (
                'name', 'parameter_end', 'effects'):
            self._start_accessor()
            return
        step = self._next_accessor_step(token)
        if step is None:
            self._reject_accessor()
        else:
            self._accessor_step = step

    def _next_accessor_step(self, token):
        name = self._accessor[0]
        step = self._accessor_step
        if step == 'name' and token == '(' \
                and name in self._ACCESSORS_WITH_PARAMETER:
            return 'parameter'
        if step == 'parameter' and token.isidentifier():
            return 'parameter_name'
        if step == 'parameter_name' and token == ')':
            return 'parameter_end'
        if step in ('name', 'effects') and name == 'get' \
                and token in self._GETTER_EFFECTS:
            return 'effects'
        if step == 'effects' and token == '(':
            return 'error_type'
        if step == 'error_type' and token not in ('{', '}'):
            return 'effects' if token == ')' else 'error_type'
        return None

    def _start_accessor(self):
        name, line = self._accessor
        self.context.push_new_function(name)
        self.context.current_function.start_line = line
        self.next(self._function_impl, '{')

    def _reject_accessor(self):
        tokens, self._accessor_tokens = self._accessor_tokens, []
        self._state = self._state_global
        for token in tokens:
            self._state(token)

    def _expect_declaration_name(self, token):
        self._previous_token = token
        self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _protocol(self, end_token):
        if end_token == "}":
            self._state = self._state_global
