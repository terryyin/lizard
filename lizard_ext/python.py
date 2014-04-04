from lizard import CodeReader


class PythonReader(CodeReader):

    ext = ['py']

    def __init__(self):
        self._state = self._GLOBAL
        self.function_stack = []

    def _GLOBAL(self, token):
        if token == 'def':
            self._state = self._FUNCTION

    def _FUNCTION(self, token):
        if token != '(':
            self.function_stack.append(self.context.current_function)
            self.context.START_NEW_FUNCTION(token)
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
        self.context.current_line -= 1
        self.context.END_OF_FUNCTION()
