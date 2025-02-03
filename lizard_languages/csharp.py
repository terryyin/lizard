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
        self.in_class_declaration = False
        self.class_name = None
        self.access_modifier = None
        self.in_primary_constructor = False
        self.current_method_name = None

    def try_new_function(self, name):
        if not self.in_primary_constructor:
            super(CSharpStates, self).try_new_function(name)
            if self.class_name and self.context.current_function:
                self.context.current_function.name = f"{self.class_name}::{name}"

    def _state_global(self, token):
        if token in ("class", "struct", "record"):
            self.in_class_declaration = True
            self.class_name = None  # Reset class name when entering a new class
            self._state = self._state_class_declaration
        elif token in ("public", "private", "protected", "internal"):
            self.access_modifier = token
        elif token == "void":
            self.current_method_name = ""
            self._state = self._state_method
        elif token[0].isalpha() or token[0] in '_~':
            if not self.in_class_declaration:
                self.try_new_function(token)
            self.access_modifier = None

    def _state_class_declaration(self, token):
        if token == '(':  # Primary constructor
            self.in_primary_constructor = True
            self._state = self._state_primary_constructor
        elif token == '{':
            self.in_class_declaration = False
            self._state = self._state_global
        elif token[0].isalpha():
            if not self.class_name:  # Only set class name if not already set
                self.class_name = token
            self._state = self._state_class_declaration
        else:
            self._state = self._state_class_declaration

    @CLikeStates.read_inside_brackets_then("()", "_state_class_declaration")
    def _state_primary_constructor(self, _):
        """Skip primary constructor parameters without counting them as a function"""
        self.in_primary_constructor = False
        pass

    def _state_method(self, token):
        if token[0].isalpha():
            self.current_method_name = token
            self.try_new_function(token)
            self._state = self._state_function
        else:
            self._state = self._state_global

    def _state_dec_to_imp(self, token):
        if token == '{':
            self.next(self._state_entering_imp, token)
        else:
            super(CSharpStates, self)._state_dec_to_imp(token)

    def _state_entering_imp(self, token):
        self.context.confirm_new_function()
        self.next(self._state_imp, token)
