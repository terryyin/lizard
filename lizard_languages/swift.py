'''
Language parser for Apple Swift
'''

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


class SwiftReplaceLabel:
    def preprocess(self, tokens):
        tokens = list(t for t in tokens if not t.isspace() or t == '\n')

        def replace_label(tokens, target, replace):
            for i in range(0, len(tokens) - len(target)):
                if tokens[i:i + len(target)] == target:
                    for j, repl in enumerate(replace):
                        tokens[i + j] = repl
            return tokens

        for k in (k for k in self.conditions if k.isalpha()):
            tokens = replace_label(tokens, ["(", k, ":"], ["(", "_" + k, ":"])
            tokens = replace_label(tokens, [",", k, ":"], [",", "_" + k, ":"])
        return tokens


class SwiftReader(CodeReader, CCppCommentsMixin, SwiftReplaceLabel):
    # pylint: disable=R0903

    FUNC_KEYWORD = 'def'
    ext = ['swift']
    language_names = ['swift']
    _conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', 'guard'])

    def __init__(self, context):
        super(SwiftReader, self).__init__(context)
        self.parallel_states = [SwiftStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CodeReader.generate_tokens(
            source_code,
            r"|`\w+`" +
            r"|\w+\?" +
            r"|\w+\!" +
            r"|\?\?" +
            addition)


class SwiftStates(GoLikeStates):  # pylint: disable=R0903
    def _state_global(self, token):
        if token in ('init', 'subscript'):
            self.context.push_new_function('')
            self.next(self._function_name, token)
        elif token in ('get', 'set', 'willSet', 'didSet', 'deinit'):
            self.context.push_new_function(token)
            self._state = self._expect_function_impl
        elif token == 'protocol':
            self._state = self._protocol
        elif token in ('let', 'var', 'case', ','):
            self._state = self._expect_declaration_name
        else:
            super(SwiftStates, self)._state_global(token)

    def _expect_declaration_name(self, token):
        self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _protocol(self, end_token):
        if end_token == "}":
            self._state = self._state_global
