'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']

    @staticmethod
    def generate_tokens(source_code, extra=''):
        extra += r"|(?:\$\w+)"
        extra += r"|(?:\<{3}(?P<quote>\w+).*?(?P=quote))"
        current_pos = 0
        code_block_pattern = re.compile(r"\<\?(?:php)?(.*?)\?\>", re.M | re.S)
        for match in code_block_pattern.finditer(source_code):
            if source_code[current_pos:match.start()]:
                yield '"' + source_code[current_pos:match.start()] + '"'
            for token in CodeReader.generate_tokens(match.group(1), extra):
                yield token
            current_pos = match.end()
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [PHPStates(context)]


class PHPStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context):
        super(PHPStates, self).__init__(context)
        # start from one, so global level will never count
        self.brace_count = 1
        self.last_tokens = ''
        self.function_name = ''
        self.function_stack = []

    def _state_global(self, token):
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
                    self._state = self._state_global
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
        self._state = self._state_global

    def _dec(self, token):
        if token == ')':
            self._state = self._state_global
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)
