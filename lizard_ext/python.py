''' Language parser for Python '''

from lizard import CodeReader


class PythonReader(CodeReader):

    ext = ['py']
    conditions = set(['if', 'for', 'while', 'and', 'or',
                      'elif', 'except', 'finally'])

    def __init__(self, context):
        super(PythonReader, self).__init__(context)
        self._state = self._global
        self.function_stack = []
        self.current_indent = 0
        self.leading_space = True

    @staticmethod
    def generate_tokens(source_code, _=None):
        return CodeReader.generate_tokens(
            source_code,
            r"|\'\'\'.*?\'\'\'" + r'|\"\"\".*?\"\"\"')

    def preprocess(self, tokens):
        for token in tokens:
            if token != '\n':
                if self.leading_space:
                    if token.isspace():
                        self.current_indent = len(token.replace('\t', ' ' * 8))
                    else:
                        if not token.startswith('#'):
                            self._close_functions()
                        self.leading_space = False
            else:
                self.leading_space = True
                self.current_indent = 0
            if not token.isspace() or token == '\n':
                yield token

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("#"):
            return token[1:]

    def _global(self, token):
        if token == 'def':
            self._state = self._function

    def _function(self, token):
        if token != '(':
            self.function_stack.append(self.context.current_function)
            self.context.start_new_function(token)
            self.context.current_function.indent = self.current_indent
        else:
            self._state = self._dec

    def _dec(self, token):
        if token == ')':
            self._state = self._state_colon
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _state_colon(self, token):
        self._state = self._state_first_line if token == ':' else self._global

    def _state_first_line(self, token):
        self._state = self._global
        if token.startswith('"""') or token.startswith("'''"):
            self.context.add_nloc(-token.count('\n') - 1)
        self._global(token)

    def eof(self):
        self.current_indent = 0
        self._close_functions()

    def _close_functions(self):
        while self.context.current_function.indent >= self.current_indent:
            endline = self.context.current_function.end_line
            self.context.end_of_function()
            self.context.current_function = self.function_stack.pop()
            self.context.current_function.end_line = endline
