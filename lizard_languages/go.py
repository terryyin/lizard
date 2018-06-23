'''
Language parser for Go lang
'''

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin


class GoReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['go']
    language_names = ['go']

    def __init__(self, context):
        super(GoReader, self).__init__(context)
        self.parallel_states = [GoStates(context)]


class GoStates(CodeStateMachine):  # pylint: disable=R0903
    def _state_global(self, token):
        if token == 'func':
            self._state = self._function_name
