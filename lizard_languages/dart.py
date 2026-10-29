'''
Language parser for Dart
'''

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class DartReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['dart']
    language_names = ['dart']

    _logical_operators = {'&&', '||', '??', '??='}

    def __init__(self, context):
        super(DartReader, self).__init__(context)
        self.parallel_states = [
            DartStates(context),
            DartNestingStackStates(context)
        ]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Longest first, subgroup-free: triple/raw strings ahead of base
        # quotes; then nullable names, ??=, ??, ?.., ?.
        # `(?!\.)` keeps int?/T? from swallowing the ? of ?. and ?..
        # Closing ?> generics stay the existing <…?> token.
        return CLikeReader.generate_tokens(
            source_code,
            r"|(?:r)?'''[\s\S]*?'''" +
            r'|(?:r)?"""[\s\S]*?"""' +
            r"|r'(?:\\.|[^'\\])*?'" +
            r'|r"(?:\\.|[^"\\])*"' +
            r"|\w+\?(?!\.)" +
            r"|\?\?=" +
            r"|\?\?" +
            r"|\?\.\." +
            r"|\?\." +
            addition,
            token_class
        )


class DartStates(CLikeStates):  # pylint: disable=R0903
    """Dart declarations: => expression bodies and get/set accessors."""

    def __init__(self, context):
        super(DartStates, self).__init__(context)
        self._expr_nesting = 0
        self._accessor_kind = None

    def _state_global(self, token):
        if token in ('get', 'set'):
            self._accessor_kind = token
            self._state = self._state_accessor_name
            return
        super(DartStates, self)._state_global(token)

    def _state_accessor_name(self, token):
        # get/set as ordinary identifiers (map.get, set.add) fall through.
        if token in ('.', '('):
            self._resume_get_or_set_as_name(token)
            return
        if token[0].isalpha() or token[0] == '_':
            is_setter = self._accessor_kind == 'set'
            self._accessor_kind = None
            name = token + ('=' if is_setter else '')
            self.context.try_new_function(name)
            if is_setter:
                self._state = self._state_function
            else:
                # Getters have no parameter list; body follows immediately.
                self._state = self._state_dec_to_imp
            return
        self._state = self._state_global
        self._resume_get_or_set_as_name(token)

    def _resume_get_or_set_as_name(self, token):
        kind = self._accessor_kind
        self._accessor_kind = None
        self.try_new_function(kind)
        self._state_function(token)

    def _state_dec_to_imp(self, token):
        if token == '=>':
            self.context.confirm_new_function()
            self._expr_nesting = 0
            self._state = self._expression_body
            return
        super(DartStates, self)._state_dec_to_imp(token)

    def _expression_body(self, token):
        # Depth-tracked so nested (e) => … and containers do not end early.
        if token in '{([':
            self._expr_nesting += 1
            return
        if token in ')]}':
            if self._expr_nesting > 0:
                self._expr_nesting -= 1
            return
        if self._expr_nesting == 0 and token == ';':
            self.context.end_of_function()
            self._state = self._state_global

    def statemachine_before_return(self):
        if self._state == self._expression_body:
            self.context.end_of_function()


class DartNestingStackStates(CLikeNestingStackStates):  # pylint: disable=R0903
    """Nesting for Dart types, including mixin and enum (not extension)."""

    _TYPE_KEYWORDS = CLikeNestingStackStates._TYPE_KEYWORDS + ("mixin", "enum")
