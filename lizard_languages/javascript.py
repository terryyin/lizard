'''
Language parser for JavaScript
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates
from .typescript import TypeScriptReader, JSTokenizer


class JavaScriptReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['js']
    language_names = ['javascript', 'js']

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
