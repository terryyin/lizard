"""JavaScript function signatures, bodies, optimistic arrow recognition,
and where an expression-bodied arrow ends."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import (
    JSXChildBrace, JSXMarkup, OPERAND_WORDS, active_js_state, ends_expression,
    parameter_group, starts_statement)

# Public API kept stable for external callers.
__all__ = (
    'OPERAND_WORDS', 'JSXChildBrace', 'JSXMarkup', 'ends_expression',
    'starts_statement', 'parameter_group', 'JavaScriptFunctionsMixin')


class JavaScriptFunctionsMixin:
    _arrow_only_signature = False

    def _read_parenthesis_group(self):
        """Only possible signatures need provisional binding frames."""
        if parameter_group(self._prev_token):
            return self._read_binding_candidate(')')
        group = self.__class__(self.context)
        group._call_group = True
        self.sub_state(group)
        return group

    def return_annotation_context(self):
        """Return ternary depth and whether the completed group proves a signature."""
        state = active_js_state(self)
        group = getattr(state, '_parenthesis_group', None)
        if (state._state == state._state_global and state.last_token == ')'
                and group is not None and parameter_group(state._parenthesis_prefix)):
            return (state._ternary_depth,
                    group._empty_parenthesis or getattr(group, '_typed_parameters', False))
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
        counted, so an ended arrow or abandoned signature keeps none of it."""
        if (self._state == self._expecting_func_opening_bracket
                and self.started_function and starts_statement(token)
                and token != '{'):
            # A line break after ")" ends the statement unless "{" opens the
            # body (`function f()\n{`). A line break before "=>" is invalid.
            self._discard_signature()
            if not self.started_function:
                return
        if (self._state == self._state_global
                and ends_expression(self.last_token)
                and starts_statement(token)):
            self._end_statement()

    def end_signature_before(self, token):
        """Called by the reader with the token after a signature's ")"
        before it is counted.  A token that cannot start the body shows that
        the signature was an expression, `x = (a)` + `if (b)`: the function
        is discarded now, so the token counts for the function around it."""
        if token == '{':
            # An initializer such as `x = (value)` needs an arrow.
            # Its following brace belongs to a separate block.
            # pylint: disable-next=comparison-with-callable
            if self._arrow_only_signature and self._state == self._expecting_func_opening_bracket:
                self._discard_signature()
                self._end_statement()
                self.next(self._expecting_statement_or_block)
            return
        if token in ('=>', ':') or (
                token == ';' and self.as_object and self._in_abstract_context):
            return      # for _expecting_func_opening_bracket
        self._discard_signature()

    def _discard_signature(self):
        """The signature was an expression, `x = (a) && b`: discard the
        function that its "(" started.  In an arrow's expression body,
        `v => o.x = (v) && b`, the "(" started none, so the arrow goes on
        with its own signature and without the parameter defaults."""
        if self._expression_body:
            function = self.context.current_function
            function.long_name, function.full_parameters = self._arrow_signature
            self.context.add_condition(-self._parameter_bindings.defaults)
        elif self.started_function:
            self._abandon_function()
        self._state = self._state_global

    def _abandon_function(self):
        """Discard an optimistic signature that proved to be an expression."""
        expression = self.context.current_function
        expression.abandoned = True     # for the extensions
        self.context.forgive = True
        self.context.end_of_function()
        self.started_function = None
        # Conditions, tokens and lines of the expression, as in
        # "x = (a && b) || c", belong to the function around it; its first
        # token and line are counted there already. Parameter defaults stay out.
        self.context.add_condition(expression.cyclomatic_complexity - 1
                                   - self._parameter_bindings.defaults)
        self.context.current_function.token_count += expression.token_count - 1
        self.context.current_function.nloc += expression.nloc - 1
        if self._expression_body:
            self._close_expression_body()

    def _abandon_optimistic_parameter_group(self):
        """Grouped expression (IIFE), not parameters; keep the open "("."""
        self._abandon_function()
        self._open_groups += 1

    def _end_grouped_function_expression(self):
        """")" closes "(() => …)" / "(function () {})", not the outer block."""
        self._open_groups -= 1
        self._pop_function_from_stack()

    def _close_expression_body(self):
        self._expression_body = False
        self.context.expression_arrows.remove(self)

    def _arrow_function(self, token):
        self._push_arrow_function()
        arrow_line = getattr(self, '_arrow_line', None)
        if (arrow_line is not None and self.started_function
                and self.context.current_function):
            # The body may start on the next line: `(e) =>\n  f(e)`.
            self.context.current_function.start_line = arrow_line
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
            # so function_name (already set) is preserved and "(" still
            # opens the parameters.
            self._consume_generic_type_params(self._function)
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
            if self._expression_body:
                function = self.context.current_function
                self._arrow_signature = (
                    function.long_name, list(function.full_parameters))
            if not self.started_function:
                self._push_function_to_stack()
                if self.started_function and not self._signature:
                    # For the extensions: this "(" can open a group.
                    self.context.current_function.optimistic = True
            self._parameter_bindings = JavaScriptBindings(
                ')', self.context.current_function)
            self._decorated_parameters = False
            self._state = self._dec
            self.context.add_to_long_function_name(" " + token)

    def _dec(self, token):
        if token == '(' and self._parameter_bindings.at_start:
            # Grouped expression, such as an arrow IIFE, rather than parameters.
            self._abandon_optimistic_parameter_group()
            self.next(self._state_global)
            self._read_binding_candidate(')')
            return
        if (token == 'async' and self._parameter_bindings.at_start
                and self.last_token == '('):
            # A grouped async arrow, `x = (async () => f())`, rather than
            # parameters; keep the open "(" as for an arrow IIFE.
            self._abandon_optimistic_parameter_group()
            self.next(self._state_global, token)
            return
        if token == '@':
            self._decorated_parameters = True  # constructor(@Inject(T) x)
        elif self._expression_operand(token):
            # `x = (items.map(f))`, `x = (g(y))`: a call or member access at
            # the root of the group, so an expression; read it as one so the
            # arrows in it are found. Re-read the name before this token.
            name = self.last_token
            self._abandon_optimistic_parameter_group()
            self.next(self._state_global, name)
            self.next(self._state_global, token)
            return
        if (token == 'function' and not self._parameter_bindings.defaults
                and not getattr(token, 'is_member_name', False)):
            # The optimistic named-arrow signature is an IIFE expression.
            self._abandon_optimistic_parameter_group()
            self.next(self._state_global, token)
            return
        if (self._parameter_bindings.at_start and self.last_token == '('
                and token.startswith('<') and len(token) > 1):
            # Parenthesized JSX: `c = (<div>…</div>)`, not parameters.
            self._abandon_optimistic_parameter_group()
            self.next(self._state_global, token)
            return
        event = self._read_parameter_binding(token)
        if event.ended:
            self._state = self._expecting_func_opening_bracket
            self.context.ended_signatures.append(self)
        elif (not event.in_expression or event.root_separator) and token != '(':
            return
        self.context.add_to_long_function_name(" " + token)

    def _expression_operand(self, token):
        bindings = self._parameter_bindings
        name = self.last_token
        return (token in ('.', '?.', '(') and not self._decorated_parameters
                and bindings.at_root and not bindings.in_expression
                and not bindings._in_parameter_type
                and name == bindings.previous and name not in ('this', 'new')
                and (name[0].isalpha() or name[0] in ('_', '$')))

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
            self._discard_signature()
        self.next(self._state_global, token)

    def _field(self, token):
        if token in ('(', '['):
            # Optional calls and subscripts retain ordinary bracket handling;
            # arguments may contain arrow functions and nested expressions.
            self.function_name = ''
            self._prev_token = '.'
            self.next(self._state_global, token)
            return
        self.last_tokens += token
        self._state = self._state_global
