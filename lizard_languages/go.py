'''
Language parser for Go lang
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .go_function import (
    GoToken, function_signature, starts_declaration, variable_declaration)
from .token_pattern import compiled_token_pattern


class GoReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['go']
    language_names = ['go']

    def __init__(self, context):
        super(GoReader, self).__init__(context)
        self.parallel_states = [GoStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition += r"|`[^`]*`"
        pattern = compiled_token_pattern(addition)

        for match in pattern.finditer(source_code):
            token = token_class(match) if token_class else match.group(0)
            if token == 'func':
                if not starts_declaration(source_code, match.end()):
                    signature = function_signature(
                        pattern.finditer(source_code, match.end()))
                    token = GoToken(token, signature)
            elif token == 'var':
                declaration = variable_declaration(
                    pattern.finditer(source_code, match.end()))
                token = GoToken(token, declaration)
            yield token

    def __call__(self, tokens, reader):
        self.context = reader.context
        state = self.parallel_states[0]
        consume = state.__call__
        for token in tokens:
            consume(token)
            yield token
        state.statemachine_before_return()
        self.eof()


class GoStates(GoLikeStates):  # pylint: disable=R0903
    """Distinguish declarations, literals and function types before opening one."""

    _array_result_line = None

    def _state_global(self, token):
        if token == 'func':
            self._start_function(token)
        elif token == 'type':
            if self.last_token != '(':
                self._state = self._type_definition
        elif token == 'var':
            self._var_name, self._signature_tokens = token.signature
            if self._signature_tokens:
                self._state = self._variable_declaration
        elif token == '{':
            self.sub_state(self.statemachine_clone())
        elif token == '}':
            self.statemachine_return()

    def _start_function(self, token, name=''):
        if isinstance(token, GoToken):
            kind, count, parameters = token.signature
        else:
            kind, count, parameters = 'declaration', 0, 0
        if kind == 'type' or (kind == 'literal' and
                              self.last_token in (']', 'chan', '*')):
            self._signature_tokens = count
            if count:
                self._state = self._function_type
        elif kind == 'literal':
            if not name:
                # Nested state machines share the per-file context.
                anonymous_count = getattr(self.context, 'go_anonymous_count', 0)
                self.context.go_anonymous_count = anonymous_count + 1
                name = f'anonymous__{anonymous_count}'
            self.context.push_new_function(name)
            self._signature_tokens = count
            self._parameter_tokens = parameters
            self._state = self._literal_signature
        elif kind == 'declaration':
            self.context.push_new_function('')
            self._state = self._function_name

    def _function_type(self, _):
        self._signature_tokens -= 1
        if not self._signature_tokens:
            self._state = self._state_global

    def _literal_signature(self, token):
        if self._parameter_tokens:
            self._parameter_tokens -= 1
            if token not in '()':
                self.context.parameter(token)
        self._signature_tokens -= 1
        if not self._signature_tokens:
            self._state = self._expect_function_impl

    def _variable_declaration(self, _):
        self._signature_tokens -= 1
        if not self._signature_tokens:
            self._state = (self._variable_initializer if self._var_name
                           else self._state_global)

    def _variable_initializer(self, token):
        self._state = self._state_global
        if token == 'func':
            self._start_function(token, self._var_name)
        else:
            self._state_global(token)

    def _expect_function_impl(self, token):
        if self._array_result_line is not None:
            if token == ';':
                self._end_array_declaration()
                return
            if (self.context.current_line > self._array_result_line
                    and self._starts_following_declaration(token)):
                function = self.context.current_function
                # The pipeline already charged the next declaration's token.
                new_line = self.context.newline
                function.token_count -= 1
                function.nloc -= new_line
                self._end_array_declaration()
                if token != 'func':
                    enclosing = self.context.current_function
                    enclosing.token_count += 1
                    enclosing.nloc += new_line
                self.next(self._state_global, token)
                return
            self._array_result_line = self.context.current_line
        if token == '[':
            self.br_count = 1
            self._state = self._array_result_length
        elif token == '{':
            self._expect_braced_function_impl(token)

    def _array_result_length(self, token):
        if token == 'func':
            # A function type and a literal can share the same prefix.
            # Keep the existing function handling when the roles are ambiguous.
            self.br_count = 0
            self._array_result_line = None
            self._state = self._expect_braced_function_impl
        elif token == '[':
            self.br_count += 1
        elif token == ']':
            self.br_count -= 1
            if not self.br_count:
                if self._array_result_line is not None:
                    self._array_result_line = self.context.current_line
                self._state = self._expect_function_impl
        elif token == '{':
            # Keep the enclosing function open and parse the contents normally.
            self._array_result_line = self.context.current_line
            self.sub_state(self.statemachine_clone())

    def _end_array_declaration(self):
        self.context.current_function.end_line = self._array_result_line
        self.context.end_of_function()
        self._array_result_line = None
        self._state = self._state_global

    @staticmethod
    def _starts_following_declaration(token):
        # A returned function or channel element type can also begin with func.
        if token == 'func':
            return not isinstance(token, GoToken) or token.signature[0] == 'declaration'
        return token in ('const', 'var', 'type', 'import', 'package')

    def _expect_braced_function_impl(self, token):
        if token == '{':
            if self.last_token in ('struct', 'interface'):
                self.next(self._inline_result_type, token)
            else:
                self._array_result_line = None
                self.next(self._function_impl, token)

    @GoLikeStates.read_inside_brackets_then('{}', '_expect_function_impl')
    def _inline_result_type(self, token):
        if not self.br_count and self._array_result_line is not None:
            self._array_result_line = self.context.current_line
