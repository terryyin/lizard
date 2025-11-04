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
