'''
Language parser for JavaScript
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates


def end_with_eof(func):
    def generate_tokens_end_with_eof(source_code, extra=""):
        tokens = func(source_code, extra)
        leading_by_word = False
        for token in tokens:
            yield token
        yield "*EOF*"
    return generate_tokens_end_with_eof


class JavaScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['js']
    language_names = ['javascript', 'js']

    @staticmethod
    @end_with_eof
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
                   r"|(?:\$\w+)" + \
                   r"|`.*?`"

        return CodeReader.generate_tokens(source_code, addition, token_class)

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
        self.parallel_states = [JavaScriptStyleLanguageStates(context)]
