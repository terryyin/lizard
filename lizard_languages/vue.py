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

    @staticmethod
    def code_tokens(tokens):
        """Only the tokens inside <script> blocks, without their tags."""
        tokens = iter(tokens)
        _skip_to_script(tokens)
        for token in tokens:
            if not token.startswith(('<script', '</script')):
                yield token
            elif token.startswith('</script'):
                _skip_to_script(tokens)


def _skip_to_script(tokens):
    for token in tokens:
        if token.startswith('<script'):
            return
