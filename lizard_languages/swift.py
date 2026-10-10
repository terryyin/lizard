'''
Language parser for Apple Swift
'''

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .swift_literals import literal_spans
from .token_pattern import compiled_token_pattern, tokens_in_span

# Backtick names, failable markers, and macros. Compiler directives stay a
# bare "#" so the shared tokenizer still keeps each directive on one line.
_SWIFT_TOKEN_ADDITION = (
    r"|`\w+`"
    r"|\w+\?"
    r"|\w+\!"
    r"|\?\?"
    r"|\#(?!(?:if|elseif|else|endif|sourceLocation|warning|error)\b)\w+"
)


def _separates_label(token):
    return token == '\n' or token.startswith('//') or token.startswith('/*')


def _next_significant(tokens, index):
    count = len(tokens)
    while index < count and _separates_label(tokens[index]):
        index += 1
    if index < count:
        return index
    return None


class SwiftReplaceLabel:
    _DECLARATION_LABELS = []

    def preprocess(self, tokens):
        tokens = [t for t in tokens if not t.isspace() or t == '\n']
        labels = {k for k in self.conditions if k.isalpha()}
        labels.update(self._DECLARATION_LABELS)
        index = 0
        count = len(tokens)
        while index < count:
            if tokens[index] not in ('(', ','):
                index += 1
                continue
            label = _next_significant(tokens, index + 1)
            if label is None or tokens[label] not in labels:
                index += 1
                continue
            colon = _next_significant(tokens, label + 1)
            if colon is not None and tokens[colon] == ':':
                tokens[label] = '_' + tokens[label]
                index = colon + 1
            else:
                index += 1
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
        pattern = compiled_token_pattern(_SWIFT_TOKEN_ADDITION + addition)
        for is_literal, start, end in literal_spans(source_code):
            if is_literal:
                yield source_code[start:end]
            else:
                yield from tokens_in_span(
                    pattern, source_code, start, end, token_class)


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

    def _next_accessor_step(self, token):  # pylint: disable=R0911
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
