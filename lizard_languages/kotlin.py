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


class KotlinStates(GoLikeStates):  # pylint: disable=R0903

    FUNC_KEYWORD = 'fun'
    _ACCESSORS = {'get', 'set'}
    _ACCESSOR_MODIFIERS = {'public', 'protected', 'internal', 'private', 'inline', 'external'}
    _EXPRESSION_BODY_ENDERS = {
        'fun', 'class', 'interface', 'val', 'var',
    }

    def __init__(self, context, in_when_cases=False, accessor_declaration_scope=None):
        super().__init__(context)
        self._in_when_cases = in_when_cases
        self._expr_nesting = 0
        self._accessor_name = None
        self._accessor_declaration_scope = (
            accessor_declaration_scope or context.global_pseudo_function
        )
        self._when_return_state = self._state_global

    def _state_global(self, token):
        if (token in self._ACCESSORS
                and self.context.current_function is self._accessor_declaration_scope):
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
                self.context.push_new_function("(anonymous)")
                self._state = super(KotlinStates, self)._expect_function_impl
        elif token in ('val', 'var', ','):
            self._state = self._expect_declaration_name
        elif token == 'interface':
            self._state = self._interface
        elif token == 'when':
            self._start_when(self._state_global)
        else:
            if token == self.FUNC_KEYWORD:
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
        self._state = self._state_global

    def _expect_function_impl(self, token):
        if token == '=':
            self._expr_nesting = 0
            self._state = self._expression_body
        elif token == '{':
            self.next(self._function_impl, token)

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

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _interface(self, end_token):
        if end_token == "}":
            self._state = self._state_global

    def _function_name(self, token):
        if token == "<":
            self.next(self._template, token)
        else:
            return super(KotlinStates, self)._function_name(token)

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
