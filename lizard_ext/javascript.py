from lizard import CodeReader


class JavaScriptReader(CodeReader):

    lan = 'JavaScript'
    ext = ['js']

    def __init__(self):
        self.brace_count = 1 # start from one, so global level will never count
        self._state = self._GLOBAL
        self.last_tokens = ''
        self.function_name = ''
        self.function_stack = []

    def _GLOBAL(self, token):
        if token == 'function':
            self._state = self._FUNCTION
        elif token in ('=', ':'):
            self.function_name = self.last_tokens
        elif token in '.':
            self._state = self._FIELD
            self.last_tokens += token
        else:
            if token == '{':
                self.brace_count += 1
            elif token == '}':
                self.brace_count -= 1
                if self.brace_count == 0:
                    self._state = self._GLOBAL
                    self.context.END_OF_FUNCTION()
                    if self.function_stack:
                        self.context.current_function = self.function_stack.pop()
                        self.brace_count = self.context.current_function.brace_count
            self.last_tokens = token
            self.function_name = ''

    def _FUNCTION(self, token):
        if token != '(':
            self.function_name = token
        else:
            self.context.current_function.brace_count = self.brace_count
            self.function_stack.append(self.context.current_function)
            self.brace_count = 0
            self.context.START_NEW_FUNCTION(self.function_name or 'function')
            self._state = self._DEC

    def _ASSIGNMENT(self, token):
        if token == 'function':
            self._state = self._FUNCTION
        self._state = self._GLOBAL

    def _FIELD(self, token):
        self.last_tokens += token
        self._state = self._GLOBAL
    
    def _DEC(self, token):
        if token == ')':
            self._state = self._GLOBAL
        else:
            self.context.PARAMETER(token)
            return
        self.context.ADD_TO_LONG_FUNCTION_NAME(" " + token)

