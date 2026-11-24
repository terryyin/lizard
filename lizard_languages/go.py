'''
Language parser for Go lang
'''

import re

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .go_function import (
    GoToken, function_signature, starts_declaration, variable_declaration)
from .token_pattern import compiled_token_pattern


_NON_GO_COMMENT_BREAK = re.compile(r'[\v\f\x1c-\x1e\x85\u2028\u2029]|\r(?!\n)')


class GoReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['go']
    language_names = ['go']

    def __init__(self, context):
        super(GoReader, self).__init__(context)
        self.parallel_states = [GoStates(context)]
        self.comment_line_offsets = []

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

    def track_comment_line_offsets(self):
        """Track line breaks that Python counts and the Go lexer ignores."""
        self.get_comment_from_token = self._counted_comment

    def source_line_changed(self, previous, current):
        """Tell whether an analysis line break includes a Go line feed."""
        line_breaks = current - previous
        for start, extra in reversed(self.comment_line_offsets):
            if start < previous:
                break
            line_breaks -= extra
        return line_breaks > 0

    def _counted_comment(self, token):
        if token.startswith(('/*', '//')):
            comment = token[2:]
            if _NON_GO_COMMENT_BREAK.search(comment):
                return _GoComment(comment, self)
            return comment
        return None


class _GoComment(str):
    def __new__(cls, comment, reader):
        token = str.__new__(cls, comment)
        token.reader = reader
        return token

    def splitlines(self, keepends=False):
        lines = super().splitlines(keepends)
        extra = max(0, len(lines) - 1) - self.count('\n')
        if extra:
            offsets = self.reader.comment_line_offsets
            start = self.reader.context.current_line
            # An earlier extension may split the same comment too.
            if not offsets or offsets[-1][0] != start:
                offsets.append((start, extra))
        return lines


class GoStates(GoLikeStates):  # pylint: disable=R0903
    """Distinguish declarations, literals and function types before opening one."""

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
