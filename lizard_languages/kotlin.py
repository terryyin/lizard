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
    _conditions = {
        'if', 'for', 'while', 'catch', '&&', '||', '?:'
    }

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

    def __init__(self, context, in_when_cases=False):
        super().__init__(context)
        self._in_when_cases = in_when_cases

    def _state_global(self, token):
        if token in ('get', 'set'):
            self.context.push_new_function(token)
            self._state = self._expect_function_impl
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
            self._state = self._when_cases
        else:
            super(KotlinStates, self)._state_global(token)

    def _expect_declaration_name(self, token):
        self._state = self._state_global

    def _expect_function_impl(self, token):
        if token == '{' or token == '=':
            self.next(self._function_impl, token)

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
            self.next(self._state_global)
        if token != '{':
            return
        self.sub_state(KotlinStates(self.context, in_when_cases=True), callback)
