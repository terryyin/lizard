"""TypeScript declarations and type syntax excluded from runtime functions."""

from .code_reader import CodeStateMachine


class TypeScriptDeclarationsMixin:
    def _handle_type_declaration(self, token):
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


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    def __init__(self, context):
        super().__init__(context)
        self.saved_token = None

    def _state_global(self, token):
        if token == '{':
            self.next(self._inline_type_annotation, token)
        else:
            self.next(self._state_simple_type, token)

    def _state_simple_type(self, token):
        if token == '<':
            self.next(self._state_generic_type, token)
        elif token in '{=;)':
            self.saved_token = token
            self.statemachine_return()
        elif token == '(':
            self.next(self._function_type_annotation, token)
        elif token == '=>':
            # Handle arrow function after type annotation
            self.saved_token = token
            self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _inline_type_annotation(self, _):
        self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("<>")
    def _state_generic_type(self, token):
        self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("()")
    def _function_type_annotation(self, _):
        self.statemachine_return()
