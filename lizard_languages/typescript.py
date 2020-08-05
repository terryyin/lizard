'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_language_states import JavaScriptStyleLanguageStates
from .js_style_regex_expression import js_style_regex_expression


class TypeScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['ts']
    language_names = ['typescript', 'ts']
    _conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                       'catch', 'case'])

    def __init__(self, context):
        super(TypeScriptReader, self).__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
            r"|(?:\w+\?)"
        return CodeReader.generate_tokens(source_code, addition, token_class)


class TypeScriptStates(JavaScriptStyleLanguageStates):

    def _expecting_func_opening_bracket(self, token):
        if token == ':':
            self.next(self._expecting_default)
            return
        super(TypeScriptStates, self)._expecting_func_opening_bracket(token)

    def _expecting_default(self, token):
        self.next(self._function_return_type)
        if token == '{':
            self.read_object()

    def _function_return_type(self, token):
        if token == ';':
            self.next(self._state_global)
        elif token == '{':
            self.next(self._expecting_func_opening_bracket, token)
