'''
Language parser for Apple Swift
'''

from .clike import CCppCommentsMixin
from .code_reader import CodeReader, CodeStateMachine
from .golike import GoLikeStates
from .swift import SwiftReplaceLabel


class KotlinReader(CodeReader, CCppCommentsMixin, SwiftReplaceLabel):
    # pylint: disable=R0903

    ext = ['kt', 'kts']
    language_names = ['kotlin']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Kotlin uses 'when' expressions, not case
    _ternary_operators = {'?:'}  # Elvis operator
    # `flow.catch { }`, `Promise::catch`, `fun catch()` and
    # `fun <T> catch()` name a member, also across a line break.
    _BEFORE_MEMBER_NAME = frozenset(('.', '::', 'fun'))
    _MEMBER_ACCESS_SPANS_LINES = True
    _TYPE_PARAMETERS_BEFORE_NAME = True

    def __init__(self, context):
        super(KotlinReader, self).__init__(context)
        self.parallel_states = [KotlinStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CodeReader.generate_tokens(
            source_code,
            r"|`\w+`" +
            r"|\w+\?" +
            r"|\w+\!!" +
            r"|\?\?" +
            r"|\?:" +
            addition
        )


class KotlinStates(GoLikeStates):  # pylint: disable=R0902,R0903

    FUNC_KEYWORD = 'fun'
    _ACCESSORS = {'get', 'set'}
    _ACCESSOR_MODIFIERS = {'public', 'protected', 'internal', 'private', 'inline', 'external'}
    _EXPRESSION_BODY_ENDERS = {
        'fun', 'class', 'interface', 'val', 'var',
    }
    _TYPE_PUNCTUATION = {'.', '?', '<', '>', ',', '(', ')', '->', '*'}
    # Words that start a declaration when they begin a line.
    _DECLARATION_STARTS = {
        '@', 'fun', 'val', 'var', 'class', 'interface', 'object', 'typealias',
        'init', 'constructor', 'companion', 'enum', 'data', 'sealed', 'annotation',
        'inner', 'value', 'open', 'abstract', 'final', 'override', 'lateinit',
        'const', 'suspend', 'inline', 'operator', 'infix', 'tailrec', 'expect',
        'actual', 'context', 'import', 'package',
    } | _ACCESSORS | _ACCESSOR_MODIFIERS
    _HEADER_OPERATORS = {':', 'where', '.', '->', ',', '<', '(', '&', 'suspend'}

    def __init__(self, context, in_when_cases=False, accessor_declaration_scope=None):
        super().__init__(context)
        self._in_when_cases = in_when_cases
        self._expr_nesting = 0
        self._accessor_name = None
        self._accessor_declaration_scope = (
            accessor_declaration_scope or context.global_pseudo_function
        )
        self._when_return_state = self._state_global
        self._start_header()

    def _state_global(self, token):
        if (token in self._ACCESSORS and self.last_token not in ('.', '::')
                and self.context.current_function is self._accessor_declaration_scope):
            self._start_header()
            self._accessor_name = token
            self.context.push_new_function(token)
            self._state = self._expect_function_impl
        elif (token in ('class', 'object') and self.last_token != '::'
              and self.context.current_function is not self.context.global_pseudo_function):
            self._state = self._local_class_header
        elif token == '->':
            if self._in_when_cases:
                self.context.add_condition()
            else:
                self._state = self._function_type_result
        elif token in ('val', 'var', ',', 'interface'):
            self._state = self._expect_declaration_name
        elif token == 'when':
            self._start_when(self._state_global)
        else:
            if token == self.FUNC_KEYWORD:
                self._start_header()
                self._accessor_name = None
            super(KotlinStates, self)._state_global(token)

    def _local_class_header(self, token):
        if token == '(':
            self.next(self._local_class_parameters, token)
        elif token == '{':
            # A local class can declare accessors while its enclosing function is active.
            self.next(self._state_global)
            self.sub_state(KotlinStates(
                self.context, accessor_declaration_scope=self.context.current_function,
            ))
        elif token in self._EXPRESSION_BODY_ENDERS or token in ('}', ';'):
            self.next(self._state_global, token)

    @CodeStateMachine.read_inside_brackets_then("()", "_local_class_header")
    def _local_class_parameters(self, token):
        pass

    def _expect_declaration_name(self, token):
        # A name after `val`, `var` or `,` is not a keyword: `val type`,
        # `(name, set)`. A lambda or a `when` argument, a parameter
        # (`, val x`) and the `->` of a `when` branch keep their role.
        if token in ('{', 'when', 'val', 'var') or (
                token == '->' and self._in_when_cases):
            self.next(self._state_global, token)
        else:
            self._state = self._state_global

    def _function_type_result(self, token):
        # Only a function type with a lambda initializer starts a function:
        # `val f: () -> Unit = { ... }`. The arrow of a lambda parameter list
        # or of a type with no lambda does not.
        if token == '=':
            self._state = self._lambda_initializer
        elif token not in self._TYPE_PUNCTUATION and not token[0].isupper():
            self.next(self._state_global, token)

    def _lambda_initializer(self, token):
        if token == '{':
            self.context.push_new_function("(anonymous)")
            self.next(self._function_impl, token)
        else:
            self.next(self._state_global, token)

    def _expect_function_impl(self, token):
        if token == '=':
            self._expr_nesting = 0
            self._state = self._expression_body
        elif token == '{':
            self.next(self._function_impl, token)
        elif self._ends_declaration(token):
            # `abstract fun f(): Int` or `private set` has no body. Its first
            # line and token were already counted in the enclosing scope.
            self.fold_into_enclosing_function()
            self.context.current_function.nloc -= 1
            self.context.current_function.token_count -= 1
            self.next(self._state_global, token)

    def _ends_declaration(self, token):
        # Kotlin semicolons are optional, so a declaration without a body
        # also ends where the next declaration starts a line, or where the
        # other accessor follows it: `private set public get() { }`.
        ends = token in (';', '}') or (not self._type_is_open() and (
            (self.context.newline and token in self._DECLARATION_STARTS)
            or (token in self._ACCESSORS and token != self._accessor_name
                and (self.last_token == self._accessor_name
                     or self.last_token in self._ACCESSOR_MODIFIERS))))
        self._track_type(token)
        return ends

    def _start_header(self):
        self._type_depth = 0
        self._type_annotated = False

    def _type_is_open(self):
        # The next token continues a type that has an open `<` or `(`, that
        # follows an operator, or that follows an annotation or a context
        # receiver: `List<out`, `T &`, `@A`, `context(String)`.
        return (self._type_depth > 0 or self._type_annotated
                or self.last_token in self._HEADER_OPERATORS)

    def _track_type(self, token):
        # An annotation keeps its role through a qualified name and its
        # arguments: `@a.B(1)`.
        if token in ('<', '('):
            if self._type_depth == 0 and self.last_token == 'context':
                self._type_annotated = True
            self._type_depth += 1
        elif token in ('>', ')'):
            self._type_depth = max(0, self._type_depth - 1)
        elif self._type_depth == 0:
            self._type_annotated = self.last_token == '@' or (
                self._type_annotated and '.' in (token, self.last_token))

    def _start_when(self, return_state):
        self._when_return_state = return_state
        self._state = self._when_cases

    def _expression_body(self, token):
        if token == 'when':
            self._start_when(self._expression_body)
            return
        if token in '{([':
            self._expr_nesting += 1
            return
        if token in ')]}':
            if self._expr_nesting > 0:
                self._expr_nesting -= 1
                return
            if token == '}':
                self._end_expression_body(token)
            return
        if self._expr_nesting == 0 and self._is_expression_body_ender(token):
            self._end_expression_body(token)

    def _is_expression_body_ender(self, token):
        if token in self._ACCESSORS:
            return self._starts_sibling_accessor(token)
        if token == 'class' and self.last_token == '::':
            return False
        return token in self._EXPRESSION_BODY_ENDERS

    def _starts_sibling_accessor(self, token):
        if self._accessor_name is None or token == self._accessor_name:
            return False
        if self.last_token in ('=', '.', '::'):
            return False
        return (self.context.newline or self.last_token == ';'
                or self.last_token in self._ACCESSOR_MODIFIERS)

    def _end_expression_body(self, token):
        self.context.end_of_function()
        self.next(self._state_global, token)

    def statemachine_before_return(self):
        if self._state == self._expression_body:
            self.context.end_of_function()

    def _function_name(self, token):
        if token == "<":
            self.next(self._template, token)
        elif token == "@":
            self._state = self._receiver_annotation
        elif not self._receiver_type_end(token):
            return super(KotlinStates, self)._function_name(token)

    def _receiver_annotation(self, _):
        self._state = self._function_name

    def _expect_function_dec(self, token):
        if not self._receiver_type_end(token):
            super()._expect_function_dec(token)

    def _receiver_type_end(self, token):
        # In `fun String.f()` and `fun List<T>?.f()`, the name read so far
        # is the receiver type. The function name comes after the dot.
        # A type argument list with a `?`, such as `<T?>`, is one token.
        if token == '.':
            self.context.current_function.name = ''
            self.context.current_function.long_name = ''
            self._state = self._function_name
        return token in ('.', '?') or token[:1] == '<' and token[-1:] == '>' and len(token) > 1

    @CodeStateMachine.read_inside_brackets_then("<>", "_function_name")
    def _template(self, tokens):
        pass

    def _when_cases(self, token):
        def callback():
            self.context.add_condition(inc=-1)
            self.next(self._when_return_state)
        if token != '{':
            return
        self.sub_state(KotlinStates(self.context, in_when_cases=True), callback)
