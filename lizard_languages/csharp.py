'''
Language parser for C Sharp
'''

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    _conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', '??'])

    def __init__(self, context):
        super(CSharpReader, self).__init__(context)
        self.parallel_states = [
            CSharpStates(context),
            CLikeNestingStackStates(context)
        ]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CLikeReader.generate_tokens(
                source_code, r"|(?:\?\?)", token_class)


class CSharpStates(CLikeStates):
    def __init__(self, context):
        super(CSharpStates, self).__init__(context)
        self.in_primary_constructor = False
        self.class_name = None

    def try_new_function(self, name):
        if not self.in_primary_constructor:
            super(CSharpStates, self).try_new_function(name)
            if self.class_name and self.context.current_function:
                self.context.current_function.name = f"{self.class_name}::{name}"

    def _state_global(self, token):
        if token in ("class", "struct", "record"):
            self.class_name = None
            self._state = self._state_class_declaration
        else:
            super(CSharpStates, self)._state_global(token)

    def _state_class_declaration(self, token):
        if token == '(':  # Primary constructor
            self.in_primary_constructor = True
            self._state = self._state_primary_constructor
        elif token == '{':
            self._state = self._state_global
        elif token[0].isalpha():
            if not self.class_name:  # Only set class name if not already set
                self.class_name = token

    @CLikeStates.read_inside_brackets_then("()", "_state_class_declaration")
    def _state_primary_constructor(self, _):
        """Skip primary constructor parameters without counting them as a function"""
        self.in_primary_constructor = False
