'''
Language parser for Solidity
'''

from .clike import CCppCommentsMixin
from .code_reader import CodeReader
from .golike import GoLikeStates


class SolidityReader(CodeReader, CCppCommentsMixin):

    ext = ['sol']
    language_names = ['solidity']
    _conditions = set(['if', 'for', 'while', '&&', '||', '?'])

    def __init__(self, context):
        super(SolidityReader, self).__init__(context)
        self.parallel_states = [SolidityStates(context)]


class SolidityStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'function'
