'''
Language parser for Vue.js files
'''

from .code_reader import CodeReader, CodeStateMachine
from .typescript import TypeScriptReader


class VueReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue', 'vuejs']

    def __init__(self, context):
        super(VueReader, self).__init__(context)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Use the base token generator but ensure we capture Vue block tags
        addition = addition + r"|(?:\<\/?\w+.*?\>)"
        return TypeScriptReader.generate_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        current_block = None

        for token in tokens:
            if token.startswith('<script'):
                current_block = 'script'
            elif token.startswith('</script'):
                current_block = None
            elif current_block == 'script':
                if not token.isspace() or token == '\n':
                    yield token
