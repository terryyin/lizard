''' Language parser for Python '''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


def count_spaces(token):
    return len(token.replace('\t', ' ' * 8))


class PythonIndents:  # pylint: disable=R0902
    def __init__(self, context):
        self.indents = [0]
        self.context = context

    def set_nesting(self, spaces, token = ""):
        while self.indents[-1] > spaces and (not token.startswith(")")):
            self.indents.pop()
            self.context.pop_nesting()
        if self.indents[-1] < spaces:
            self.indents.append(spaces)
            self.context.add_bare_nesting()

    def reset(self):
        self.set_nesting(0)


class PythonReader(CodeReader, ScriptLanguageMixIn):

    ext = ['py']
    language_names = ['python']
    _conditions = set(['if', 'for', 'while', 'and', 'or',
                      'elif', 'except', 'finally'])

    def __init__(self, context):
        super(PythonReader, self).__init__(context)
        self.parallel_states = [PythonStates(context, self)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return ScriptLanguageMixIn.generate_common_tokens(
                source_code,
                r"|\'\'\'.*?\'\'\'" + r'|\"\"\".*?\"\"\"', token_class)

    def preprocess(self, tokens):
        indents = PythonIndents(self.context)
        current_leading_spaces = 0
        reading_leading_space = True
        for token in tokens:
            if token != '\n':
                if reading_leading_space:
                    if token.isspace():
                        current_leading_spaces += count_spaces(token)
                    else:
                        if not token.startswith('#'):
                            current_function = self.context.current_function
                            if current_function.name == '*global*' or current_function.long_name.endswith(')'):
                                indents.set_nesting(current_leading_spaces, token)
                        reading_leading_space = False
            else:
                reading_leading_space = True
                current_leading_spaces = 0
            if not token.isspace() or token == '\n':
                yield token
        indents.reset()


class PythonStates(CodeStateMachine):  # pylint: disable=R0903
    def __init__(self, context, reader):
        super(PythonStates, self).__init__(context)
        self.reader = reader

    def _state_global(self, token):
        if token == 'def':
            self._state = self._function

    def _function(self, token):
        if token != '(':
            self.context.restart_new_function(token)
            self.context.add_to_long_function_name("(")
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
