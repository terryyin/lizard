'''
Language parser for TSX
'''

from .typescript import TypeScriptReader, TypeScriptStates
from .javascript import JSTokenizer
from .jsx import JSXMixin
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression


class TSXReader(TypeScriptReader, JSXMixin):
    # pylint: disable=R0903

    ext = ['tsx']
    language_names = ['tsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        return JSXMixin.generate_tokens(source_code, addition, token_class)

    def __init__(self, context):
        super(TSXReader, self).__init__(context)
        self.parallel_states = [TSXStates(context)]


class TSXStates(TypeScriptStates):
    def _expecting_func_opening_bracket(self, token):
        if token == '<':
            self.next(self._expecting_jsx)
            return
        super(TSXStates, self)._expecting_func_opening_bracket(token)

    def _expecting_jsx(self, token):
        if token == '>':
            self.next(self._expecting_func_opening_bracket) 