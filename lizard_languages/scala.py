'''
Language parser for Scala
'''
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin, CLikeReader
__author__ = 'David Baum'


class ScalaReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['scala']
    language_names = ['scala']
    conditions = set(['if', 'else', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', 'do'])

    def __init__(self, context):
        super(ScalaReader, self).__init__(context)
        self.parallel_states = [ScalaStates(context)]


class ScalaStates(CodeStateMachine):  # pylint: disable=R0903
    def _state_global(self, token):
        if token == 'def':
            self._state = self._function_name
        if token == 'main':
            self._function_name(token)

    def _function_name(self, token):
        self.context.start_new_function(token)
        self._state = self._expect_function_dec

    def _expect_function_dec(self, token):
        if token == '(':
            self._state = self._function_dec

    def _function_dec(self, token):
        if token == ')':
            self._state = self._expect_function_impl
        else:
            self.context.parameter(token)

    def _expect_function_impl(self, token):
        if token == '{':
            self._state = self._function_impl
            self._state(token)

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _function_impl(self, _):
        self._state = self._state_global
        self.context.end_of_function()

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _protocol(self, end_token):
        if end_token == "}":
            self._state = self._state_global
