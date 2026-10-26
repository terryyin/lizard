"""JavaScript function signatures, bodies, and optimistic arrow recognition."""

from .javascript_bindings import JavaScriptBindings


# An expression goes on at the next line after one of these tokens, or at a
# line that starts with one of the second ones.
_CONTINUED_AFTER = frozenset((
    '=>', '=', '+', '-', '*', '/', '%', '&&', '||', '??', '?', ':', '|', '&',
    '^', '==', '===', '!=', '!==', '<', '>', '<=', '>='))
_CONTINUED_BY = frozenset((
    '.', '?.', '?', ':', '&&', '||', '??', '|', '&', '^', '%', '==', '===',
    '!=', '!==', '<=', '>=', '+', '-', '/', 'instanceof', 'in'))


# TypeScript type keywords that should not be counted as parameters
_TS_TYPE_KEYWORDS = frozenset([
    'string', 'number', 'boolean', 'void', 'any',
    'object', 'unknown', 'never',
])


class JavaScriptFunctionsMixin:
    def _push_function_to_stack(self):
        if self._in_abstract_context:
            return
        self.started_function = True
        self.context.push_new_function(self.function_name or '(anonymous)')

    def _pop_function_from_stack(self):
        if self.started_function:
            self.context.end_of_function()
        self.started_function = None
        self._in_prop_value = False
        self._expression_body = False

    def _continues_expression(self, token):
        return token in _CONTINUED_BY or self.last_token in _CONTINUED_AFTER

    def _end_expression_body(self):
        """End an arrow function without braces before the first token of a
        new line, which belongs to the code around it."""
        arrow = self.context.current_function
        counted, complexity = self._counted
        self._pop_function_from_stack()
        if counted is not arrow:
            return
        around = self.context.current_function
        around.cyclomatic_complexity += arrow.cyclomatic_complexity - complexity
        arrow.cyclomatic_complexity = complexity
        arrow.nloc -= 1
        around.nloc += 1
        arrow.token_count -= 1
        around.token_count += 1
        arrow.end_line = self._last_line
        around.end_line = self.context.current_line

    def _abandon_function(self):
        """Discard an optimistic signature that proved to be an expression."""
        self.context.forgive = True
        self.context.end_of_function()
        self.started_function = None

    def _arrow_function(self, token):
        self._push_arrow_function()
        # Clear function_name so expression-body ( doesn't re-enter _function
        self.function_name = ''
        # Clear modifiers so the body's opening { isn't captured by the
        # async/static handler in the class body path.
        self._async_seen = False
        self._static_seen = False
        # Without braces, a later "{" opens an object: x => c ? {a} : {b}
        self._expression_body = token != '{'
        self.next(self._state_global, token)

    def _function(self, token):
        if token == '*':
            return
        if token == '<':
            # Generic type params: function name<T>(...) — consume <...>
            # so function_name (already set) is preserved.
            self._consume_generic_type_params()
            return
        if token.startswith('<') and token.endswith('>') and len(token) > 1:
            # Single-token generic from TSX tokenizer (e.g., <T>, <Props>)
            return
        if token != '(':
            # Only set function_name for valid identifiers
            if token and (token[0].isalpha() or token[0] in ('_', '$', '#')):
                self.function_name = token
            else:
                self.function_name = ''
            # Reset modifiers after setting function name
            self._static_seen = False
            self._async_seen = False
        else:
            if not self.started_function:
                self._push_function_to_stack()
            self._generic_depth_in_dec = 0
            self._parameter_bindings = JavaScriptBindings(')')
            self._state = self._dec
            self.context.add_to_long_function_name(" " + token)

    def _dec(self, token):
        if token == '(' and self._parameter_bindings.at_start:
            # Grouped expression, such as an arrow IIFE, rather than parameters.
            self._abandon_function()
            self.next(self._state_global)
            self._read_binding_candidate(')')
            return
        if token == 'function' and not self._parameter_bindings.defaults:
            # The optimistic named-arrow signature is an IIFE expression.
            self._abandon_function()
            self.next(self._state_global, token)
            return
        event = self._read_parameter_binding(token)
        if event.ended:
            self._state = self._expecting_func_opening_bracket
        elif (not event.in_expression or event.root_separator) and token != '(':
            # Filter out TypeScript type keywords and operators from parameter count
            if token == ',':
                # Ignore commas inside generic type brackets: Map<K, V>
                if event.root_separator and not getattr(self, '_generic_depth_in_dec', 0):
                    self.context.parameter(',')
            elif token == '<':
                self._generic_depth_in_dec = getattr(
                    self, '_generic_depth_in_dec', 0) + 1
            elif token == '>':
                depth = getattr(self, '_generic_depth_in_dec', 0)
                if depth > 0:
                    self._generic_depth_in_dec = depth - 1
            elif token in _TS_TYPE_KEYWORDS:
                pass
            elif token in ('*', '+', '-', '/', '%', '=', '.'):
                pass
            elif not getattr(self, '_generic_depth_in_dec', 0):
                if (token.replace('_', '').replace('?', '').isalnum() and
                        token.replace('?', '') and
                        token.replace('?', '')[0].isalpha()):
                    self.context.parameter(token.replace('?', ''))
            return
        self.context.add_to_long_function_name(" " + token)

    def _expecting_func_opening_bracket(self, token):
        # Do not reset started_function for arrow functions (=>)
        if token == ':':
            self.next(self._state_global)
            self._consume_type_annotation()
            return
        elif token == ';' and self.as_object and self._in_abstract_context:
            # Abstract method declaration ends with ';' — no body
            if self.started_function:
                self._pop_function_from_stack()
            self._in_abstract_context = False
            self.next(self._state_global)
        elif token != '{' and token != '=>':
            if self.started_function:
                self._abandon_function()
        self.next(self._state_global, token)
