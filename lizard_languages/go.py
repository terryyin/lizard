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

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition + r"|`[^`]*`"  # Add support for backtick-quoted strings
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            # Skip counting ? in backtick-quoted strings
            if token.startswith('`') and token.endswith('`'):
                for state in self.parallel_states:
                    state(token)
                yield token
                continue

            # For non-backtick tokens, process normally
            for state in self.parallel_states:
                state(token)
            yield token
        for state in self.parallel_states:
            state.statemachine_before_return()
        self.eof()


class GoStates(GoLikeStates):  # pylint: disable=R0903
    pass
