'''
Language parser for Vue.js files
'''

from .code_reader import CodeReader, CodeStateMachine
from .typescript import TypeScriptReader


class VueReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue', 'vuejs']
    code_block_open, code_block_close = '<script', '</script'

    def __init__(self, context):
        super(VueReader, self).__init__(context)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Use the base token generator but ensure we capture Vue block tags
        addition = addition + r"|(?:\<\/?\w+.*?\>)"
        return TypeScriptReader.generate_tokens(source_code, addition, token_class)
