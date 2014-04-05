from lizard import CodeReader


class PythonReader(CodeReader):

    ext = ['py']

    def __init__(self):
        self._state = self._GLOBAL
        self.function_stack = []
        self.current_indent = 0
        self.newline = True

    def preprocess(self, tokens, context):
        for token in tokens:
            if token != '\n':
                if self.newline:
                    self._close_functions(len(token) if token.isspace() else 0)
            self.newline = token == '\n'
            if not token.isspace() or token == '\n':
                yield token

    def _GLOBAL(self, token):
        if token == 'def':
            self._state = self._FUNCTION

    def _FUNCTION(self, token):
        if token != '(':
            self.function_stack.append(self.context.current_function)
            self.context.START_NEW_FUNCTION(token)
            self.context.current_function.indent = self.current_indent
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
        self._close_functions(0)

    def _close_functions(self, new_indent):
        self.current_indent = new_indent
        while self.context.current_function.indent >= self.current_indent:
            endline = self.context.current_function.end_line
            self.context.END_OF_FUNCTION()
            self.context.current_function = self.function_stack.pop()
            self.context.current_function.end_line = endline
