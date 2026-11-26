'''
Language parser for Solidity
'''

from .clike import CCppCommentsMixin
from .code_reader import CodeReader
from .golike import GoLikeStates


class SolidityReader(CodeReader, CCppCommentsMixin):

    ext = ['sol']
    language_names = ['solidity']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(SolidityReader, self).__init__(context)
        self.parallel_states = [SolidityStates(context)]


class SolidityStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'function'

    SPECIAL_FUNCTIONS = ('constructor', 'receive', 'fallback')

    def _state_global(self, token):
        if (token in self.SPECIAL_FUNCTIONS and self.context.current_function
                is self.context.global_pseudo_function):
            self._special_name = token
            self._state = self._special_function
        else:
            super()._state_global(token)

    def _special_function(self, token):
        # Only a contract member declaration `receive(...)` is an entry
        # point; `receive` is otherwise an ordinary identifier.
        if token == '(':
            self.context.push_new_function(self._special_name)
            self.next(self._function_dec, token)
        else:
            self.next(self._state_global, token)
