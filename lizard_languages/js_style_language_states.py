'''
"JavaScript style language" includes JavaScript and PHP
'''

from .code_reader import CodeStateMachine


class JavaScriptStyleLanguageStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context):
        super(JavaScriptStyleLanguageStates, self).__init__(context)
        # start from one, so global level will never count
        self.brace_count = 1
        self.last_tokens = ''
        self.function_name = ''
        self.function_stack = []

    # Generator callback function.  These are the function that run when the
    # generator is itterating through the token list from the source code.
    # This function will run with the next token when `self._state = _state_global`
    def _state_global(self, token):
        if token == 'function' or token == '=>':
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
                if self.brace_count <= 0:
                    self._state = self._state_global
                    self._pop_function_from_stack()
            self.last_tokens = token
            self.function_name = ''

    # Generator callback function.  These are the function that run when the
    # generator is itterating through the token list from the source code.
    # This function will run with the next token when `self._state = _function`
    def _function(self, token):
        if self.last_token == '=>':
            self._start_new_function('() =>')
        elif token != '(':
            self.function_name = token
        else:
            self._start_new_function()

    # Generator callback function.  These are the function that run when the
    # generator is itterating through the token list from the source code.
    # This function will run with the next token when `self._state = _field`
    def _field(self, token):
        self.last_tokens += token
        self._state = self._state_global

    # Generator callback function.  These are the function that run when the
    # generator is itterating through the token list from the source code.
    # This function will run with the next token when `self._state = _dec`
    def _dec(self, token):
        if token in (')', '}', ';'):
            self._state = self._state_global
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _start_new_function(self, function_name='function'):
        self.context.current_function.brace_count = self.brace_count
        self.function_stack.append(self.context.current_function)
        self.brace_count = 0
        self.context.start_new_function(self.function_name or function_name)
        if function_name == '() =>':
            self.context.parameter('s')
            self._state = self._state_global
        else:
            self._state = self._dec

    def _pop_function_from_stack(self):
        self.context.end_of_function()
        if self.function_stack:
            self.context.current_function = self.function_stack.pop()
            self.brace_count = self.context.current_function.brace_count
