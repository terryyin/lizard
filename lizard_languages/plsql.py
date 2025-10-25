"""
Language parser for PL/SQL (Oracle's Procedural Language extension to SQL)

This module implements complexity analysis for PL/SQL code, supporting:
- Procedures, Functions, and Triggers
- Package Bodies (not specifications - they only contain signatures)
- Nested procedures and functions
- Anonymous blocks with nested functions (blocks themselves aren't counted)
- Control structures: IF/ELSIF/ELSE, CASE/WHEN, LOOP/WHILE/FOR
- Exception handlers
- Cursor declarations and cursor FOR loops

Design Decisions:
- EXIT WHEN: The WHEN keyword is filtered out by the preprocessor because
  "EXIT WHEN condition" is not a branching construct - it's a conditional
  exit that doesn't create alternate execution paths. The LOOP itself adds
  complexity.

- CONTINUE WHEN: Similar to EXIT WHEN, the WHEN is counted as it does create
  a branch in the loop execution.

- GOTO: Does not add to cyclomatic complexity as it's just an unconditional
  jump, not a decision point.

- Standalone LOOP: Adds +1 complexity as it creates a repeating path.

- FOR/WHILE LOOP: The FOR/WHILE keyword adds complexity; the following LOOP
  keyword is part of the same construct and doesn't add additional complexity.

- Parameter Counting: Currently counts all non-whitespace tokens and commas
  in parameter lists. This approach works but differs from some other language
  implementations.
"""

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin


class PLSQLReader(CodeReader, CCppCommentsMixin):
    """
    Reader for PL/SQL language supporting procedures, functions, packages,
    and core control structures.
    """

    ext = ["sql", "pks", "pkb", "pls", "plb", "pck"]
    language_names = ["plsql", "pl/sql"]

    # PL/SQL conditions for cyclomatic complexity
    # Note: 'loop' is NOT in this set because LOOP has special handling:
    # - standalone LOOP adds +1
    # - LOOP after WHILE/FOR should not add (it's part of the compound statement)
    _conditions = {"if", "elsif", "when", "while", "for", "and", "or"}

    def __init__(self, context):
        super(PLSQLReader, self).__init__(context)
        self.parallel_states = [PLSQLStates(context)]
        # PL/SQL is case-insensitive, so add both lowercase and uppercase versions
        # of keywords to the conditions set for the automatic condition counter
        self.conditions = self.conditions | {c.upper() for c in self.conditions}

    def preprocess(self, tokens):
        """
        Preprocess tokens to handle PL/SQL-specific constructs.
        Merge compound keywords to prevent the condition counter from double-counting:
        - "END IF", "END LOOP", "END CASE", "END WHILE", "END FOR" -> single tokens
        - "EXIT WHEN" -> remove the WHEN keyword (EXIT doesn't create a branch)
        """
        last_nonwhitespace_token = None
        pending_tokens = []

        for token in tokens:
            if not token.isspace() or token == "\n":
                token_upper = token.upper()

                # Handle "END IF", "END LOOP", etc.
                if (
                    last_nonwhitespace_token
                    and last_nonwhitespace_token.upper() == "END"
                ):
                    if token_upper in ("IF", "LOOP", "CASE", "WHILE", "FOR"):
                        # Merge into "END_IF", "END_LOOP", etc.
                        yield "END_" + token_upper
                        last_nonwhitespace_token = None
                        pending_tokens = []
                        continue

                # Handle "EXIT WHEN" - skip the WHEN keyword
                if (
                    last_nonwhitespace_token
                    and last_nonwhitespace_token.upper() == "EXIT"
                    and token_upper == "WHEN"
                ):
                    # Skip this WHEN keyword
                    pending_tokens = []
                    continue

                # Yield any pending tokens
                if last_nonwhitespace_token:
                    yield last_nonwhitespace_token
                for pending in pending_tokens:
                    yield pending
                pending_tokens = []

                # Update tracking
                last_nonwhitespace_token = token
            else:
                # Accumulate whitespace
                pending_tokens.append(token)

        # Don't forget the last tokens
        if last_nonwhitespace_token:
            yield last_nonwhitespace_token
        for pending in pending_tokens:
            yield pending

    @staticmethod
    def generate_tokens(source_code, addition="", token_class=None):
        """
        Generate tokens for PL/SQL code.
        PL/SQL uses:
        - Single-line comments: --
        - Multi-line comments: /* */
        - String literals: 'text' (with '' for escaping)
        - Assignment operator: :=
        """
        # Add PL/SQL-specific patterns
        addition = r"|--[^\n]*" + addition  # Single-line comment starting with --
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def get_comment_from_token(self, token):
        """
        Override to recognize PL/SQL's -- line comments in addition to /* */ block comments.
        PL/SQL uses -- for single-line comments (like SQL standard).

        Note: This method correctly identifies -- comments, but due to a limitation in
        the NLOC calculation, these comments may still be counted in NLOC.
        """
        if token.startswith("--"):
            return token  # Return full comment token (like Lua does)
        # Delegate to parent for /* */ and // comments
        return super().get_comment_from_token(token)


class PLSQLStates(CodeStateMachine):
    """
    State machine for parsing PL/SQL code structure.
    """

    def __init__(self, context):
        super(PLSQLStates, self).__init__(context)
        self.in_parameter_list = False
        self.last_control_keyword = None  # Track FOR/WHILE to avoid counting their LOOP
        self.declaring_nested_function = (
            False  # Track if we're declaring a nested function
        )

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
            self.in_parameter_list = True
            self.next(self._parameters, "(")
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
            self.in_parameter_list = True
            self.next(self._parameters, "(")
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
            self.in_parameter_list = True
            self.next(self._parameters, "(")
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
            self.in_parameter_list = True
            self.next(self._parameters, "(")
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

    def _parameters(self, token):
        """Read parameters."""
        if token == ")":
            self.in_parameter_list = False
            self.next(self._after_parameters)
        elif token == ",":
            # Each comma separates parameters
            self.context.parameter(token)
        elif not token.isspace() and token != "\n":
            # Track non-whitespace tokens as potential parameters
            self.context.parameter(token)

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

    def _state_body(self, token):
        """
        Process function/procedure body.
        Track control structures for cyclomatic complexity.
        Manually track BEGIN/END blocks.
        """
        token_lower = token.lower()
        token_upper = token.upper()

        # Check for merged compound keywords like "END_IF", "END_LOOP", etc.
        # These are created by the preprocessor
        if token_lower.startswith("end_"):
            # This is a compound END keyword, reset tracking
            self.last_control_keyword = None
            return

        # Handle nested procedure/function declarations
        if token_lower == "procedure":
            self.next(self._procedure_name)
            return
        elif token_lower == "function":
            self.next(self._function_name)
            return

        # Track FOR and WHILE to know when LOOP follows them
        if token_upper in ("FOR", "WHILE"):
            self.last_control_keyword = token_upper

        # Handle LOOP keyword manually
        # - Standalone LOOP adds +1 complexity
        # - LOOP after FOR/WHILE does not add complexity (already counted for FOR/WHILE)
        elif token_upper == "LOOP":
            if self.last_control_keyword not in ("FOR", "WHILE"):
                # This is a standalone LOOP, add complexity
                self.context.add_condition()
            # Reset tracking after processing LOOP
            self.last_control_keyword = None

        # PL/SQL uses BEGIN/END instead of {}
        if token_lower == "begin":
            self.br_count += 1
            self.context.add_bare_nesting()
        elif token_lower == "end":
            # This is a standalone END (for BEGIN/END block)
            self.br_count -= 1
            if self.br_count == 0:
                # This END closes the function/procedure
                # Check if we have a parent function BEFORE ending (stack gets popped)
                has_parent = len(self.context.stacked_functions) > 0
                self.context.end_of_function()
                # Return to appropriate state based on whether this was nested
                if has_parent:
                    # Return to parent function's declaration section
                    self.next(self._state_before_begin)
                else:
                    # No parent function, return to global
                    self.next(self._state_global)
                return
            else:
                self.context.pop_nesting()

        # Note: Basic conditions (if, elsif, when, while, for, and, or)
        # are automatically counted by the condition_counter processor
        # based on the _conditions set in the Reader class.
