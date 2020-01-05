'''
Language parser for Scala
'''
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
__author__ = 'David Baum'


class ScalaReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['scala']
    language_names = ['scala']
    _conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', 'do'])

    def __init__(self, context):
        super(ScalaReader, self).__init__(context)
        self.parallel_states = [ScalaStates(context)]


class ScalaStates(GoLikeStates):  # pylint: disable=R0903

    FUNC_KEYWORD = 'def'

    def _state_global(self, token):
        super(ScalaStates, self)._state_global(token)

    def _expect_function_impl(self, token):
        if token == "=":
            self._state = self._expect_function_body
        else:
            super(ScalaStates, self)._expect_function_impl(token)

    def _expect_function_body(self, token):
        if self.context.newline:
            self.context.end_of_function()
            self.next(self._state_global, token)
        elif token == '{':
            self.sub_state(ScalaStates(self.context))

    def statemachine_before_return(self):
        if self._state == self._expect_function_body:
            self.context.end_of_function()
