'''
Language parser for TSX
'''

from .typescript import TypeScriptReader, TypeScriptStates, JSTokenizer
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
        # No need for parallel states since JSX handling is in the mixin
