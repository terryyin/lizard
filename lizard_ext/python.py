from lizard import CodeReader


class PythonReader(CodeReader):

    ext = ['py']

    def __init__(self):
        self._state = self._GLOBAL
        self.function_stack = []
        self.function_indent = -1
        self.current_indent = 0
        self.newline = True

    def preprocess(self, tokens, context):
        for token in tokens:
            if token.isspace() and token != '\n':
                if self.newline:
                    self.current_indent = len(token)
                    if self.current_indent <= self.function_indent:
                        self.context.END_OF_FUNCTION()
                        self.function_indent = -1
            else:
                self.newline = token == '\n'
                yield token

    def _GLOBAL(self, token):
        if token == 'def':
            self._state = self._FUNCTION

    def _FUNCTION(self, token):
        if token != '(':
            self.function_stack.append(self.context.current_function)
            self.context.START_NEW_FUNCTION(token)
            self.function_indent = self.current_indent
        else:
            self._state = self._DEC

    def _DEC(self, token):
        if token == ')':
            self._state = self._GLOBAL
        else:
            self.context.PARAMETER(token)
            return
        self.context.ADD_TO_LONG_FUNCTION_NAME(" " + token)

    def eof(self):
        if self.function_indent >= 0:
            self.context.END_OF_FUNCTION()
