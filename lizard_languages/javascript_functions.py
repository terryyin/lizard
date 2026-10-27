"""JavaScript function signatures, bodies, optimistic arrow recognition,
and where an expression-bodied arrow ends."""

from .javascript_bindings import JavaScriptBindings

# Words that still need an operand, so a line break after them continues.
OPERAND_WORDS = frozenset((
    'in', 'instanceof', 'typeof', 'new', 'await', 'void', 'delete', 'yield',
    'of', 'as', 'satisfies', 'keyof'))


def ends_expression(token):
    """Whether an expression may end with this token (JavaScript ASI)."""
    first = token[:1]
    if first.isalnum() or first in ('_', '$', '#', "'", '"', '`'):
        return token not in OPERAND_WORDS
    return (token in (')', ']', '}', '++', '--')
            or (first == '/' and len(token) > 2)  # regular expression literal
            or (first == '<' and (token.startswith('</')  # JSX element end
                                  or token.endswith('/>'))))


def starts_statement(token):
    """Whether this line-start token cannot continue the previous line."""
    first = token[0]
    if first.isalnum() or first in ('_', '$', '#', "'", '"'):
        # TypeScript reads `as`/`satisfies` after a line break as a statement.
        return token not in ('in', 'instanceof')
    # Restricted ++/--, prefix-only operators, blocks, decorators and closers.
    return token in ('++', '--', '!', '~', '{', '@', '}', ')', ']')


def parameter_group(prefix):
    return (prefix is None or prefix in ('return', 'yield', 'await', 'async')
            or (not ends_expression(prefix) and prefix not in ('.', '?.', 'new')))


class JavaScriptFunctionsMixin:
    def return_annotation_context(self):
        """Use the active state's completed group before counting a colon."""
        state = self
        while isinstance(state._state, self.__class__):
            state = state._state
        group = getattr(state, '_parenthesis_group', None)
        if (state._state == state._state_global and state.last_token == ')'
                and group is not None and parameter_group(state._parenthesis_prefix)):
            return state._ternary_depth, group._empty_parenthesis
        return None

    def _push_function_to_stack(self):
        if self._in_abstract_context:
            return
        self.started_function = True
        self.context.push_new_function(self.function_name or '(anonymous)')

    def _pop_function_from_stack(self):
        if self.started_function:
            self.context.end_of_function()
        if self._expression_body:
            self._close_expression_body()
        self.started_function = None
        self._in_prop_value = False

    def end_expression_before(self, token):
        """Called by the reader with a line's first token before it is
        counted, so an ended arrow keeps none of the next statement."""
        if (self._state == self._state_global
                and ends_expression(self.last_token)
                and starts_statement(token)):
            self._end_statement()

    def _abandon_function(self):
        """Discard an optimistic signature that proved to be an expression."""
        self.context.forgive = True
        self.context.end_of_function()
        self.started_function = None
        if self._expression_body:
            self._close_expression_body()

    def _close_expression_body(self):
        self._expression_body = False
        self.context.expression_arrows.remove(self)

    def _arrow_function(self, token):
        self._push_arrow_function()
        if token == '{':
            # A block body closes any expression body (`a => b => {}`).
            if self._expression_body:
                self._close_expression_body()
        elif self.started_function and not self._expression_body:
            # An expression body has no closing token, so a later "{" is an
            # object operand; the reader asks the innermost open one at each
            # line start (end_expression_before). Function-type "=>" must not
            # reach here (see type annotation states).
            self._expression_body = True
            # Nested curried arrows reuse this function and keep its owner.
            self._expression_ternary_depth = self._ternary_depth
            self.context.expression_arrows.append(self)
        # Clear function_name so expression-body ( doesn't re-enter _function
        self.function_name = ''
        # Clear modifiers so the body's opening { isn't captured by the
        # async/static handler in the class body path.
        self._async_seen = False
        self._static_seen = False
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
            self._in_parameter_type = False
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
        if token == 'this' and self._parameter_bindings.at_start:
            # TypeScript's leading receiver declaration is not an argument.
            self._in_parameter_type = True
        event = self._read_parameter_binding(token)
        if event.ended:
            self._state = self._expecting_func_opening_bracket
        elif (not event.in_expression or event.root_separator) and token != '(':
            if token == ',':
                # Ignore commas inside generic type brackets: Map<K, V>
                if event.root_separator and not getattr(self, '_generic_depth_in_dec', 0):
                    self._in_parameter_type = False
                    self.context.parameter(',')
            elif token == ':' and self._parameter_bindings.at_root:
                self._in_parameter_type = True
            elif token == '<':
                self._generic_depth_in_dec = getattr(
                    self, '_generic_depth_in_dec', 0) + 1
            elif token == '>':
                depth = getattr(self, '_generic_depth_in_dec', 0)
                if depth > 0:
                    self._generic_depth_in_dec = depth - 1
            elif token in ('*', '+', '-', '/', '%', '=', '.'):
                pass
            elif not (getattr(self, '_generic_depth_in_dec', 0)
                      or self._in_parameter_type):
                name = token.replace('?', '')
                if name.replace('$', '_').isidentifier():
                    self.context.parameter(name)
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
