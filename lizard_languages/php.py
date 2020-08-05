'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_language_states import JavaScriptStyleLanguageStates


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']
    _conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                       'catch', 'case'])

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition += r"|(?:\$\w+)"
        addition += r"|(?:\<{3}(?P<quote>\w+).*?(?P=quote))"
        current_pos = 0
        code_block_pattern = re.compile(
                r"\<\?(?:php)?(.*?)(?:(\?\>)|\Z)",
                re.M | re.S)
        for match in code_block_pattern.finditer(source_code):
            if source_code[current_pos:match.start()]:
                yield '"' + source_code[current_pos:match.start()] + '"'
            for token in CodeReader.generate_tokens(
                    match.group(1), addition, token_class):
                yield token
            current_pos = match.end()
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [JavaScriptStyleLanguageStates(context)]
