"""TypeScript declarations and type syntax excluded from runtime functions."""

from .typescript_type_annotation import TypeScriptTypeAnnotationStates
from .javascript_functions import OPERAND_WORDS, ends_expression
from .js_style_regex_context import _EXPRESSION_KEYWORDS


# Words that start what _handle_type_declaration handles; after `declare`
# it also handles the next token.
DECLARATION_WORDS = frozenset((
    'declare', 'type', 'interface', 'abstract', 'class', 'const', 'let', 'var',
    'as', 'satisfies'))
# Type-only contextual words can be identifiers in runtime operand position.
_EXPRESSION_PREFIXES = (
    _EXPRESSION_KEYWORDS | OPERAND_WORDS | {'default'}
) - {'as', 'satisfies', 'keyof'}


class TypeScriptDeclarationsMixin:
    def _handle_type_declaration(self, token):
        if token in ('const', 'let', 'var'):
            self._declaration_binding = True
            self.last_tokens = self._prev_token = token
            return True
        elif token == 'class' and (not self.as_object or self._in_prop_value):
            self._class_body_pending = True
        elif (token in ('as', 'satisfies')
              and (not self.as_object or self._in_prop_value)
              and self._assertion_operand()):
            self._consume_type_annotation()
            return True
        if token == 'declare':
            self._ts_declare = True
            return True
        if token == 'function' and getattr(self, '_ts_declare', False):
            # Skip declared function
            self._ts_declare = False
            # Skip tokens until semicolon or newline

            def skip_declared_function(t):
                if t == ';' or self.context.newline:
                    self.next(self._state_global)
                    return True
                return False
            self.next(skip_declared_function)
            return True
        self._ts_declare = False

        # Skip type alias declarations: type Name = { ... }
        # These contain arrow signatures that are not runtime functions.
        if token == 'type' and not self.as_object:
            phase = [0]        # 0=expect name, 1=expect =, 2=after =
            brace_count = [0]
            generic_depth = [0]

            def handle_type_alias(t):
                if phase[0] == 0:
                    if t and t[0].isalpha():
                        phase[0] = 1
                    else:
                        self.last_tokens = 'type'
                        self.next(self._state_global)
                        self._state_global(t)
                        return True
                elif phase[0] == 1:
                    if t == '<':
                        generic_depth[0] = 1
                        phase[0] = 3
                    elif t == '=':
                        phase[0] = 2
                    elif t == ';':
                        self.next(self._state_global)
                        return True
                elif phase[0] == 2:
                    if t == '{':
                        brace_count[0] = 1
                        phase[0] = 4
                    elif t == ';' or self.context.newline:
                        self.next(self._state_global)
                        if t != ';':
                            self._state_global(t)
                        return True
                elif phase[0] == 3:
                    if t == '<':
                        generic_depth[0] += 1
                    elif t == '>':
                        generic_depth[0] -= 1
                        if generic_depth[0] == 0:
                            phase[0] = 1
                elif phase[0] == 4:
                    if t == '{':
                        brace_count[0] += 1
                    elif t == '}':
                        brace_count[0] -= 1
                        if brace_count[0] == 0:
                            self.next(self._state_global)
                            return True
                return False

            self.next(handle_type_alias)
            return True

        # Skip interface declarations — method signatures are not runtime functions
        if token == 'interface':
            brace_count = 0
            interface_started = False

            def skip_interface(t):
                nonlocal brace_count, interface_started
                if t == '{':
                    interface_started = True
                    brace_count += 1
                elif t == '}' and interface_started:
                    brace_count -= 1
                    if brace_count == 0:
                        self.next(self._state_global)
                        return True
                return False

            self.next(skip_interface)
            return True

        # Track abstract modifier inside class bodies
        if token == 'abstract' and self.as_object:
            self._in_abstract_context = True
            return True
        return False

    def _assertion_operand(self):
        """Assertions follow consumed operands, not expression/statement starts."""
        if (self.context.newline or not self._prev_token
                or self._declaration_binding is True or self._class_body_pending):
            return False
        token = self.last_token or ''
        if token == '!':
            return getattr(self, '_postfix_operand', False)
        return ((self._prev_token == '.' or token not in _EXPRESSION_PREFIXES)
                and (ends_expression(token) or token.isidentifier()))

    def _consume_generic_type_params(self):
        """Consume <...> generic type parameters (e.g., method<T>(...))
        so the method name in last_tokens is preserved."""
        depth = 1

        def consume(token):
            nonlocal depth
            if token == '<':
                depth += 1
            elif token == '>':
                depth -= 1
                if depth == 0:
                    self.next(self._state_global)
        self.next(consume)

    def _consume_type_annotation(self):
        typeStates = TypeScriptTypeAnnotationStates(self.context)

        def callback():
            if typeStates.saved_token:
                self._replay_token(typeStates.saved_token)
        self.sub_state(typeStates, callback)

    def _read_runtime_colon(self):
        binding = self._binding_candidate
        if (self._declaration_binding is True or binding
                and binding._parameters is not None
                and binding.at_root and not binding.in_expression):
            self._typed_parameters = True
            self._consume_type_annotation()
        else:
            # An unclaimed runtime colon introduces a labeled statement.
            self.next(self._expecting_statement_or_block)
        self._prev_token = ':'
