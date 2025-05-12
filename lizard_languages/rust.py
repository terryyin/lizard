'''
Language parser for Rust lang
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


class RustReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['rs']
    language_names = ['rust']
    _conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', 'match', 'where'])

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [RustStates(context)]


class RustStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'fn'
