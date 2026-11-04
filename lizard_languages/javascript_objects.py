"""JavaScript object and class members, including computed method names."""

from .javascript_functions import ends_expression


class JavaScriptObjectsMixin:
    def _typed_class_member(self):
        return (self.as_class and not self._in_prop_value
                and self.context.class_field_annotations)

    def optional_member_context(self):
        state = self
        while isinstance(state._state, self.__class__):
            state = state._state
        return state._state == state._state_global and state._typed_class_member()

    def _end_field_before_modifier(self):
        """A modifier at a line start begins the next class member, so a
        field value written without ';' ends before it, as before a name."""
        if (self.as_class and self._in_prop_value and self.context.newline
                and ends_expression(self.last_token)):
            self._end_statement()

    def _optional_class_member(self):
        if not self._typed_class_member():
            return False
        self.context.add_condition(-1)
        return True

    def _handle_object_member(self, token):
        """Called for tokens in an object literal or class body (as_object)."""
        if token == '=' and self.as_class:
            self._in_prop_value = True
        if (self.as_class and token.endswith('?')
                and self._typed_class_member()):
            self.last_tokens = token[:-1]
            return True
        # Support for getter/setter: look for 'get' or 'set' before method name
        if token == '...':
            # What a spread takes is a value: ...(c && {f() {}}), ...g(x)
            self._in_prop_value = True
            self.last_tokens = token
            return True
        if token in ('get', 'set'):
            self._end_field_before_modifier()
            self._getter_setter_prefix = token
            return True
        if self._getter_setter_prefix:
            prefix, self._getter_setter_prefix = self._getter_setter_prefix, None
            if token not in ('(', ':', ',', '}', '=', ';', '<'):
                # Next token is the property name
                self.last_tokens = f"{prefix} {token}"
                return True
            # A member named "get" or "set": get(key) {}, {get: f}, {get, set}
            self.last_tokens = prefix
        if token == '[':
            if self._in_prop_value:
                # An array value, {list: [[1], 2]}, not a computed name.
                self._read_binding_candidate(']')
            else:
                self._collect_computed_name()
            return True
        if token == ':' and self.as_class and self._typed_class_member():
            self.function_name = self.last_tokens
            self._consume_type_annotation()
            return True
        if token == ':':
            # Only set function_name for valid identifiers
            name = self.last_tokens
            if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                self.function_name = name
            self._in_prop_value = True
            return True
        elif (token == '<' or token.startswith('<') and token.endswith('>')) and (
                not self._in_prop_value or self.last_token in (':', '=')):
            # Methods and the start of arrow values can have type parameters.
            # Inside a value, an infix '<' is not a method signature.
            if token == '<':
                self._consume_generic_type_params()
            return True
        elif token == '(':
            # Check if this is a method call (previous token was . or new)
            if self._prev_token == '.' or self._prev_token == 'new':
                # Method call inside object — use its group reader so
                # the matching ')' doesn't escape the object reader.
                self._read_parenthesis_group()
                self._prev_token = token
                return True
            # In property value (after ':'), identifier( is a function call
            # unless it's the prop name itself: prop: (...) => {} is arrow fn
            if self._in_prop_value and self.last_tokens != '=' and (
                    not self.function_name
                    or self.last_tokens != self.function_name):
                self._read_parenthesis_group()
                self._prev_token = token
                return True
            if not self.started_function:
                # When last_tokens is '=' we're in a field assignment
                # pattern (field = () => {}), so use function_name which
                # was set by the '=' handler to the field name.
                if self.last_tokens == '=' and self.function_name:
                    self._function(self.function_name)
                else:
                    self._function(self.last_tokens)
            self.next(self._function, token)
            return True
        # If we've seen async/static and this is an identifier, it's likely a method name
        elif (self._async_seen or self._static_seen) and token not in ('*', 'function', '=>'):
            if token == '=':
                # End of static/async field name — clear modifiers so
                # the value expression and subsequent members parse
                # normally.  e.g. `static propTypes = { ... };`
                self._static_seen = False
                self._async_seen = False
                # Fall through to the general '=' handler below
            else:
                # This is a method name after async/static
                self.last_tokens = token
                return True
        return False

    def read_object(self):
        def callback(summary):
            self._pending_bindings = summary
            self.next(self._state_global)

        object_reader = self._read_binding_candidate(
            '}', callback, as_object=True, track_complexity=False)
        object_reader.as_class = self._class_body_pending
        self._class_body_pending = False
        object_reader._static_seen = self._static_seen
        object_reader._async_seen = self._async_seen
        self._static_seen = False
        self._async_seen = False

    def _collect_computed_name(self):
        # Collect tokens between [ and its ]
        tokens = []
        depth = [0]

        def collect(token):
            depth[0] += {'[': 1, ']': -1}.get(token, 0)
            if depth[0] < 0:
                # Try to join tokens and camelCase if possible
                name = ''.join(tokens)
                # Remove quotes and pluses for simple cases
                name = name.replace("'", '').replace('"', '').replace('+', '').replace(' ', '')
                # Lowercase first char, uppercase next word's first char
                name = self._to_camel_case(name)
                self.last_tokens = name
                self.next(self._state_global)
                return True
            tokens.append(token)
            return False
        self.next(collect)

    def _to_camel_case(self, s):
        # Simple camelCase conversion for test case
        if not s:
            return s
        parts = s.split()
        if not parts:
            return s
        return parts[0][0].lower() + parts[0][1:] + ''.join(p.capitalize() for p in parts[1:])
