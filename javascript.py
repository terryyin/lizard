from lizard import CLikeReader
class JavaScriptReader(CLikeReader):
    
    def _GLOBAL(self, token):
        if token in ('=', ':'):
            self._state = self._ASSIGNMENT
        elif token == '.':
            self._state = self._FIELD
            self.context.ADD_TO_FUNCTION_NAME(token)
        else:
            CLikeReader._GLOBAL(self, token)

    def _ASSIGNMENT(self, token):
        self._state = self._GLOBAL

    def _FIELD(self, token):
        self.context.ADD_TO_FUNCTION_NAME(token)
        self._state = self._GLOBAL


