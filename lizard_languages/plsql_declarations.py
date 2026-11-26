"""
PL/SQL declaration states: recognizing procedure, function, and trigger
headers (top-level and nested) up to the handoff into body tracking.
"""


class PLSQLDeclarationStates:
    """
    Declaration-header states mixed into the PL/SQL state machine.
    The host state machine supplies `_state_body`.
    """

    def _state_global(self, token):
        """Global state - looking for function/procedure/trigger declarations."""
        token_lower = token.lower()

        if token_lower == "procedure":
            self.next(self._procedure_name)
        elif token_lower == "function":
            self.next(self._function_name)
        elif token_lower == "trigger":
            self.next(self._trigger_name)

    def _procedure_name(self, token):
        """Read procedure name."""
        if token.isspace() or token == "\n":
            return
        if token == "(":
            self._start_parameters()
        elif token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        else:
            # Check if this is a nested function
            if self.declaring_nested_function:
                self.context.push_new_function(token)
                self.declaring_nested_function = False
            else:
                self.context.try_new_function(token)
            self.next(self._procedure_after_name)

    def _procedure_after_name(self, token):
        """After procedure name, look for parameters or IS/AS."""
        if token == ".":
            # Schema-qualified name: the previous token was the schema,
            # next non-whitespace token will be the actual procedure name
            self.next(self._procedure_name_after_dot)
        elif token == "(":
            self._start_parameters()
        elif token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        # Skip whitespace and other tokens

    def _procedure_name_after_dot(self, token):
        """Read the actual procedure name after schema.dot."""
        if token.isspace() or token == "\n":
            return
        # Replace the previous (schema) name with the actual procedure name
        self.context.current_function.name = token
        self.next(self._procedure_after_name)

    def _function_name(self, token):
        """Read function name."""
        if token.isspace() or token == "\n":
            return
        if token == "(":
            self._start_parameters()
        elif token.lower() == "return":
            self.next(self._return_type)
        elif token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        else:
            # Check if this is a nested function
            if self.declaring_nested_function:
                self.context.push_new_function(token)
                self.declaring_nested_function = False
            else:
                self.context.try_new_function(token)
            self.next(self._function_after_name)

    def _function_after_name(self, token):
        """After function name, look for parameters, RETURN, or IS/AS."""
        if token == ".":
            # Schema-qualified name: the previous token was the schema,
            # next non-whitespace token will be the actual function name
            self.next(self._function_name_after_dot)
        elif token == "(":
            self._start_parameters()
        elif token.lower() == "return":
            self.next(self._return_type)
        elif token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        # Skip whitespace and other tokens

    def _function_name_after_dot(self, token):
        """Read the actual function name after schema.dot."""
        if token.isspace() or token == "\n":
            return
        # Replace the previous (schema) name with the actual function name
        self.context.current_function.name = token
        self.next(self._function_after_name)

    def _return_type(self, token):
        """Skip return type declaration."""
        if token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        # Skip everything else (return type tokens)

    def _start_parameters(self):
        """Begin reading a formal parameter list."""
        self._param_paren_depth = 0
        self._param_tokens = []
        self.context.add_to_long_function_name("(")
        self.next(self._parameters)

    def _commit_parameter(self):
        """Record the buffered formal parameter declaration, if any."""
        if not self._param_tokens:
            return
        name = self._param_tokens[0]
        declaration = " ".join(self._param_tokens)
        self.context.add_parameter_declaration(name, declaration)
        self._param_tokens = []

    def _parameters(self, token):
        """Read parameters, keeping nested parentheses inside one formal."""
        if token.isspace():
            return
        if token == "(":
            self._param_paren_depth += 1
            self._param_tokens.append(token)
        elif token == ")":
            if self._param_paren_depth > 0:
                self._param_paren_depth -= 1
                self._param_tokens.append(token)
            else:
                self._commit_parameter()
                self.context.add_to_long_function_name(")")
                self.next(self._after_parameters)
        elif token == "," and self._param_paren_depth == 0:
            self._commit_parameter()
        else:
            self._param_tokens.append(token)

    def _after_parameters(self, token):
        """After parameters, look for IS/AS or RETURN."""
        if token.lower() == "return":
            self.next(self._return_type)
        elif token.lower() in ("is", "as"):
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        # Skip whitespace and other tokens

    def _trigger_name(self, token):
        """Read trigger name."""
        if token.isspace() or token == "\n":
            return
        # Trigger name found
        self.context.try_new_function(token)
        self.seen_trigger_name_token = False  # Track if we've seen non-whitespace after name
        self.next(self._trigger_after_name)

    def _trigger_after_name(self, token):
        """After trigger name, skip until DECLARE or BEGIN."""
        token_lower = token.lower()

        # Only check for dot immediately after trigger name (before any other tokens)
        if token == "." and not self.seen_trigger_name_token:
            # Schema-qualified name: the previous token was the schema,
            # next non-whitespace token will be the actual trigger name
            self.next(self._trigger_name_after_dot)
            return

        # Mark that we've seen a non-whitespace token after the trigger name
        if not token.isspace() and token != "\n":
            self.seen_trigger_name_token = True

        if token_lower == "declare":
            self.context.confirm_new_function()
            self.next(self._state_before_begin)
        elif token_lower == "begin":
            self.context.confirm_new_function()
            self.br_count = 1
            self.next(self._state_body)
        # Skip everything else (BEFORE/AFTER, INSERT/UPDATE/DELETE, ON table_name, FOR EACH ROW, etc.)

    def _trigger_name_after_dot(self, token):
        """Read the actual trigger name after schema.dot."""
        if token.isspace() or token == "\n":
            return
        # Replace the previous (schema) name with the actual trigger name
        self.context.current_function.name = token
        self.seen_trigger_name_token = False  # Reset for the real trigger name
        self.next(self._trigger_after_name)

    def _state_before_begin(self, token):
        """
        State between IS/AS and BEGIN - this is the declaration section.
        Watch for nested procedures/functions and the BEGIN keyword.
        """
        token_lower = token.lower()

        # Check for nested procedure/function declarations
        if token_lower == "procedure":
            self.declaring_nested_function = True
            # Store current br_count level to know when nested function ends
            if not hasattr(self, "nested_br_level"):
                self.nested_br_level = 0
            self.next(self._procedure_name)
            return
        elif token_lower == "function":
            self.declaring_nested_function = True
            # Store current br_count level to know when nested function ends
            if not hasattr(self, "nested_br_level"):
                self.nested_br_level = 0
            self.next(self._function_name)
            return
        elif token_lower == "begin":
            # Start of the implementation body
            # Check if we had nested functions and need to reset br_count tracking
            if hasattr(self, "nested_br_level"):
                self.br_count = self.nested_br_level + 1
                delattr(self, "nested_br_level")
            else:
                self.br_count = 1  # Initialize counter for the first BEGIN
            self.next(self._state_body)
