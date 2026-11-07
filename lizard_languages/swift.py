'''
Language parser for Apple Swift
'''

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .swift_conditions import SwiftConditionLists
from .swift_literals import literal_spans, string_parts
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


class _OptionalQuestionMark(str):
    """Source punctuation whose Swift lexical role is postfix, not ternary."""

    is_optional = True


_POSTFIX_OPTIONAL = _OptionalQuestionMark('?')


def _swift_token(match):
    token = match.group(0)
    if token == '?':
        source, start = match.string, match.start()
        # Comments have the same lexical role as whitespace in Swift.
        if start and not source[start - 1].isspace() and not source.endswith(
                '*/', 0, start):
            return _POSTFIX_OPTIONAL
    return token


def _separates_label(token):
    return token == '\n' or token.startswith('//') or token.startswith('/*')


def _next_significant(tokens, index):
    count = len(tokens)
    while index < count and _separates_label(tokens[index]):
        index += 1
    if index < count:
        return index
    return None


def _is_local_name(token):
    return token.isidentifier() or (
        len(token) > 2 and token[0] == token[-1] == '`')


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
            if colon is not None and _is_local_name(tokens[colon]):
                # A declaration may give the label a separate local name:
                # `func f(for value: Int)`.
                colon = _next_significant(tokens, colon + 1)
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
        self._condition_lists = SwiftConditionLists(context)

    def process_token(self, token):
        scanner = self._condition_lists
        # Ordinary tokens only remember the previous one. Braces, condition
        # keywords, and an already-open list do the real scan.
        if (scanner._lists or scanner._closed_repeat or token == '{'
                or token == '}' or token in scanner._STARTS):
            scanner(token)
        else:
            scanner._previous = token
        if token is _POSTFIX_OPTIONAL:
            self.context.add_condition(-1)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        pattern = compiled_token_pattern(_SWIFT_TOKEN_ADDITION + addition)
        if token_class is None and '?' in source_code:
            token_class = _swift_token
        return _swift_tokens(
            pattern, token_class, source_code, 0, len(source_code))


def _swift_tokens(pattern, token_class, source, start, end):
    for is_literal, span_start, span_end in literal_spans(source, start, end):
        if not is_literal:
            yield from tokens_in_span(
                pattern, source, span_start, span_end, token_class)
        elif source.startswith('/', span_start) or source.find(
                '\\', span_start, span_end) < 0:
            yield source[span_start:span_end]
        else:
            # Literal text stays whole. An interpolation's parentheses are
            # code tokens, so brackets stay balanced for the scanners that
            # track them; the pieces still join into the literal.
            for is_code, part_start, part_end in string_parts(
                    source, span_start, span_end):
                if is_code:
                    yield from _swift_tokens(
                        pattern, token_class, source, part_start, part_end)
                elif part_start < part_end:
                    yield source[part_start:part_end]


class _PropertyHeader:  # pylint: disable=R0903
    '''A `var` or `let` declaration read up to its body or initializer.'''

    def __init__(self, keyword):
        self.keyword = keyword
        self.name = self.name_line = self.line = None
        self.nloc = self.tokens = self.depth = 0

    def named(self, name, context):
        self.name = name
        self.name_line = self.line = context.current_line
        self.nloc = context.current_function.nloc
        self.tokens = context.current_function.token_count


class SwiftStates(GoLikeStates):  # pylint: disable=R0903

    TYPE_KEYWORD = None

    # Failable initializers stay one token so `?` is not counted as a ternary.
    _FAILABLE_INITIALIZERS = {'init?': 'init', 'init!': 'init'}
    _ACCESSORS = {'get', 'set', 'willSet', 'didSet', 'deinit'}
    _ACCESSORS_WITH_PARAMETER = {'set', 'willSet', 'didSet'}
    _GETTER_EFFECTS = {'async', 'throws'}
    _ACCESSOR_BLOCK_STARTS = _ACCESSORS | {
        'mutating', 'nonmutating', '_read', '_modify', 'init', '@', '}'}
    _TYPE_CONTINUATIONS = {'{', '=', '->', '&', '.', '?', '!'}
    # A type annotation is incomplete after these, so it continues on the
    # next line.
    _TYPE_OPERATORS = {':', '->', '&', '.', '<', '(', '[', ',', '@'}

    def __init__(self, context):
        super(SwiftStates, self).__init__(context)
        self._previous_token = None
        self._accessor = None
        self._accessor_step = None
        self._accessor_tokens = []
        self._property = None

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
        elif token in ('let', 'var') and self._previous_token != '.' \
                and self.context.current_function is \
                self.context.global_pseudo_function:
            self._property = _PropertyHeader(token)
            self._state = self._property_name
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

    def _property_name(self, token):
        # A computed property or a closure-initialized property is code
        # that runs on access, so it is reported as a function named after
        # the property. Locals inside functions stay with the function.
        self._previous_token = token
        if _is_local_name(token):
            self._property.named(token, self.context)
            self._state = self._property_after_name
        else:
            self._state = self._state_global

    def _property_after_name(self, token):
        self._previous_token = token
        if token == ':':
            self._property.line = self.context.current_line
            self._state = self._property_type
        elif token == '=':
            self._state = self._property_initializer
        else:
            self.next(self._state_global, token)

    def _property_type(self, token):
        header = self._property
        depth = header.depth
        if depth == 0 and (token in (',', ';', '}', ')', ']') or (
                self.context.current_line != header.line
                and self._previous_token not in self._TYPE_OPERATORS
                and token not in self._TYPE_CONTINUATIONS)):
            self.next(self._state_global, token)
        elif depth == 0 and token == '{':
            if header.keyword != 'var':
                self.next(self._state_global, token)
                return
            self._start_property_function()
            self._state = self._computed_property_body
        elif depth == 0 and token == '=':
            self._state = self._property_initializer
        else:
            if token in ('(', '[', '<'):
                header.depth += 1
            elif token in (')', ']', '>'):
                header.depth -= 1
            header.line = self.context.current_line
        self._previous_token = token

    def _property_initializer(self, token):
        if token == '{':
            self._start_property_function()
            self.next(self._function_impl, token)
        else:
            self.next(self._state_global, token)

    def _computed_property_body(self, token):
        if token in self._ACCESSOR_BLOCK_STARTS:
            # `{ get ... }` or `{ didSet ... }`: the braces hold
            # accessors, which are functions of their own.
            self.fold_into_enclosing_function()
            # Without a function, lines and tokens stay counted once.
            self.context.current_function.nloc -= 1
            self.context.current_function.token_count -= 1
            self._state = self._state_global
            self.sub_state(self.statemachine_clone(), None, token)
        else:
            self._function_impl('{')
            self(token)

    def _start_property_function(self):
        # The function starts at the property name: lines and tokens read
        # since then move from the enclosing scope to the function.
        header = self._property
        enclosing = self.context.current_function
        nloc = enclosing.nloc - header.nloc
        tokens = enclosing.token_count - header.tokens
        enclosing.nloc -= nloc
        enclosing.token_count -= tokens
        self.context.push_new_function(header.name)
        function = self.context.current_function
        function.start_line = header.name_line
        function.nloc += nloc
        function.token_count += tokens

    def _expect_declaration_name(self, token):
        self._previous_token = token
        self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _protocol(self, end_token):
        if end_token == "}":
            self._state = self._state_global
