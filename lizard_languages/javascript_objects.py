"""JavaScript object and class members, including computed method names."""


class JavaScriptObjectsMixin:
    def _handle_object_member(self, token):
        if not self.as_object:
            return False
        # Support for getter/setter: look for 'get' or 'set' before method name
        if token in ('get', 'set'):
            self._getter_setter_prefix = token
            return True
        if self._getter_setter_prefix:
            # Next token is the property name
            self.last_tokens = f"{self._getter_setter_prefix} {token}"
            self._getter_setter_prefix = None
            return True
        if token == '[':
            self._collect_computed_name()
            return True
        if token == ':':
            # Only set function_name for valid identifiers
            name = self.last_tokens
            if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                self.function_name = name
            self._in_prop_value = True
            return True
        elif token == '<' or (
                token.startswith('<') and token.endswith('>') and len(token) > 1):
            # Generic type params on method: sortByKey<T>(...) {
            # Handles both multi-token <T, U> and single-token <T> from TSX tokenizer.
            if token == '<':
                self._consume_generic_type_params()
            return True
        elif token == '(':
            # Check if this is a method call (previous token was . or new)
            if self._prev_token == '.' or self._prev_token == 'new':
                # Method call inside object — use sub_state so
                # the matching ')' doesn't escape the object reader.
                self.sub_state(self.__class__(self.context))
                self._prev_token = token
                return True
            # In property value (after ':'), identifier( is a function call
            # unless it's the prop name itself: prop: (...) => {} is arrow fn
            if self._in_prop_value and (
                    not self.function_name
                    or self.last_tokens != self.function_name):
                self.sub_state(self.__class__(self.context))
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
        object_reader._static_seen = self._static_seen
        object_reader._async_seen = self._async_seen
        self._static_seen = False
        self._async_seen = False

    def _collect_computed_name(self):
        # Collect tokens between [ and ]
        tokens = []

        def collect(token):
            if token == ']':
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
