'''
Language parser for Go lang
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


class GoReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['go']
    language_names = ['go']

    def __init__(self, context):
        super(GoReader, self).__init__(context)
        self.parallel_states = [GoStates(context)]


class GoStates(GoLikeStates):  # pylint: disable=R0903
    pass
