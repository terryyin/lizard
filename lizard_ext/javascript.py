'''
Language parser for JavaScript
'''

from lizard import CodeReader, CCppCommentsMixin
import re


class JavaScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['js']

    @staticmethod
    def generate_tokens(source_code, _=None):
        regx_regx = r"|/(?:\\.|[^/])+?/[igm]*"
        regx_pattern = re.compile(regx_regx)
        word_pattern = re.compile(r'\w+')
        tokens = CodeReader.generate_tokens(source_code, regx_regx)
        leading_by_word = False
        for token in tokens:
            if leading_by_word and regx_pattern.match(token):
                for subtoken in CodeReader.generate_tokens(token):
                    yield subtoken
            else:
                yield token
            if not token.isspace():
                leading_by_word = word_pattern.match(token)

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
        # start from one, so global level will never count
        self.brace_count = 1
        self._state = self._global
        self.last_tokens = ''
        self.function_name = ''
        self.function_stack = []

    def _global(self, token):
        if token == 'function':
            self._state = self._function
        elif token in ('=', ':'):
            self.function_name = self.last_tokens
        elif token in '.':
            self._state = self._field
            self.last_tokens += token
        else:
            if token == '{':
                self.brace_count += 1
            elif token == '}':
                self.brace_count -= 1
                if self.brace_count == 0:
                    self._state = self._global
                    self._pop_function_from_stack()
            self.last_tokens = token
            self.function_name = ''

    def _pop_function_from_stack(self):
        self.context.end_of_function()
        if self.function_stack:
            self.context.current_function = self.function_stack.pop()
            self.brace_count = self.context.current_function.brace_count

    def _function(self, token):
        if token != '(':
            self.function_name = token
        else:
            self.context.current_function.brace_count = self.brace_count
            self.function_stack.append(self.context.current_function)
            self.brace_count = 0
            self.context.start_new_function(self.function_name or 'function')
            self._state = self._dec

    def _field(self, token):
        self.last_tokens += token
        self._state = self._global

    def _dec(self, token):
        if token == ')':
            self._state = self._global
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)
