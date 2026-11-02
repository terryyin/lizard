'''
Language parser for Vue.js files
'''

import re

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

    def code_tokens(self, tokens):
        """Script tokens only; keep newline count of every filtered token."""
        in_script = False
        for token in tokens:
            if token.startswith('<script'):
                in_script = True
                self.context.class_field_annotations = bool(re.search(
                    r'''\blang\s*=\s*(['"]?)(?:ts|tsx|typescript)\1(?=[\s>])''',
                    token, re.IGNORECASE))
            elif token.startswith('</script'):
                in_script = False
            elif in_script:
                yield token
                continue
            for _ in range(token.count('\n')):
                yield '\n'
