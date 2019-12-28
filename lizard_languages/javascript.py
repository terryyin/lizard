'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates


class JavaScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['js']
    language_names = ['javascript', 'js']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
                   r"|(?:\$\w+)" + \
                   r"|(?:\<\w+\>)" + \
                   r"|(?:\<\/\w+\>)" + \
                   r"|`.*?`"

        xml_openning = re.compile(r"\<(\w+)\>")
        xml_closing = re.compile(r"\<\/(\w+)\>")
        saved = ''
        openning = None
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            mch = xml_openning.match(token)
            if not openning and mch:
                openning = mch[1]
                saved = token
                continue
            mch = xml_closing.match(token)
            if mch and mch[1] == openning:
                yield saved + token
                saved = ''
                continue
            if saved:
                saved += token
            else:
                yield token

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
        self.parallel_states = [JavaScriptStyleLanguageStates(context)]
