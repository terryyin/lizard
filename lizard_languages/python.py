''' Language parser for Python '''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn
import itertools


class PythonReader(CodeReader, ScriptLanguageMixIn):

    ext = ['py']
    language_names = ['python']
    conditions = set(['if', 'for', 'while', 'and', 'or',
                      'elif', 'except', 'finally'])
    loops = set(['if', 'for', 'while', 'and', 'or', 'else', 'try',
                'elif', 'except', 'finally'])
    loop_indicator = ':'
    indent_indicator = '<indent>'

    def __init__(self, context):
        super(PythonReader, self).__init__(context)
        self.parallel_states = [PythonStates(context, self)]
        self.current_indent = 0
        self.function_stack = []
        self.array_indent = [0]
        self.indent_indicator = '<indent>'

    @staticmethod
    def generate_tokens(source_code, _=None):
        return CodeReader.generate_tokens(
            source_code,
            r"|\'\'\'.*?\'\'\'" + r'|\"\"\".*?\"\"\"')

    def preprocess(self, tokens):
        leading_space = True
        for token in tokens:
            if token != '\n':
                if leading_space:
                    if token.isspace():
                        self.current_indent = len(token.replace('\t', ' ' * 8))
                        if self.array_indent[-1] != 0:
                            indent_number = (self.array_indent[-1] - self.current_indent)
                            for _ in itertools.repeat(None, indent_number / 4):
                                yield self.indent_indicator
                        self.array_indent.append(self.current_indent)
                    else:
                        if not token.startswith('#'):
                            self._close_functions()
                        leading_space = False
            else:
                leading_space = True
                self.current_indent = 0
            if not token.isspace() or token == '\n':
                yield token

    def _close_functions(self):
        while self.context.current_function.indent >= self.current_indent:
            endline = self.context.current_function.end_line
            self.context.end_of_function()
            self.context.current_function = self.function_stack.pop()
            self.context.current_function.end_line = endline

    def eof(self):
        self.current_indent = 0
        self._close_functions()


class PythonStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context, reader):
        super(PythonStates, self).__init__(context)
        self.reader = reader

    def _state_global(self, token):
        if token == 'def':
            self._state = self._function

    def _function(self, token):
        if token != '(':
            self.reader.function_stack.append(self.context.current_function)
            self.context.start_new_function(token)
            self.context.current_function.indent = self.reader.current_indent
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
        if token == ':':
            self.next(self._state_first_line)
        else:
            self.next(self._state_global)

    def _state_first_line(self, token):
        self._state = self._state_global
        if token.startswith('"""') or token.startswith("'''"):
            self.context.add_nloc(-token.count('\n') - 1)
        self._state_global(token)
