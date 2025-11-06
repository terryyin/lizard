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

    # Separated condition categories
    # Note: 'loop' is NOT in this set to avoid double-counting FOR LOOP and WHILE LOOP
    # Standalone LOOP is handled manually by the state machine
    _control_flow_keywords = {"if", "elsif", "when", "while", "for"}
    _logical_operators = {"and", "or"}
    _case_keywords = set()  # PL/SQL uses 'when' in case expressions
    _ternary_operators = set()

    def __init__(self, context):
        super(PLSQLReader, self).__init__(context)
        self.parallel_states = [PLSQLStates(context)]
        # PL/SQL is case-insensitive, so add both lowercase and uppercase versions
        # of keywords to the conditions set for the automatic condition counter
        self.conditions = self.conditions | {c.upper() for c in self.conditions}

    def _normalize_token(self, token):
        """
        Normalize PL/SQL tokens for cognitive complexity compatibility.
        - Convert keywords to lowercase
        - Map BEGIN -> { and END -> } for nesting
        - Map ELSIF -> elif for consistency
        - Map AND/OR to lowercase for binary operator sequences
        """
        token_upper = token.upper()
        token_lower = token.lower()

        # Map BEGIN to { for cognitive complexity nesting
        if token_upper == "BEGIN":
            return "{"

        # Map standalone END to } (compound ENDs like END_IF already handled)
        elif token_upper == "END":
            return "}"

        # Normalize keywords that affect complexity to lowercase
        # (cognitive_complexity_counter expects lowercase)
        # IMPORTANT: Keep ELSIF as "elsif" not "elif" for condition_counter compatibility
        elif token_upper in ("IF", "ELSIF", "WHILE", "FOR", "ELSE", "WHEN", "AND", "OR",
                            "CASE", "EXCEPTION", "TRY", "CATCH"):
            return token_lower

        # Keep everything else as-is
        else:
            return token

    def preprocess(self, tokens):
        """
        Preprocess tokens to handle PL/SQL-specific constructs.

        For Cyclomatic Complexity:
        - Merge "END IF", "END LOOP", etc. to prevent double-counting
        - Remove WHEN from "EXIT WHEN" (EXIT doesn't create a branch)

        For Cognitive Complexity compatibility:
        - Normalize keywords to lowercase (cogc_counter expects lowercase)
        - Map BEGIN -> { and END -> } for nesting tracking
        - Map ELSIF -> elif for consistency with other languages
        - Emit synthetic { after LOOP to increase nesting (FOR/WHILE LOOP constructs)
        """
        last_nonwhitespace_token = None
        pending_tokens = []
        saw_for_or_while = False  # Track if we just saw FOR or WHILE
        in_exception_block = False  # Track if we're in an EXCEPTION block
        in_case_expression = False  # Track if we're in a CASE expression
        saw_if_then = False  # Track if we've emitted { for current IF statement

        for token in tokens:
            if not token.isspace() or token == "\n":
                token_upper = token.upper()

                # Track CASE keyword - WHEN after this acts like switch case, not if
                if token_upper == "CASE":
                    in_case_expression = True
                # Track EXCEPTION keyword - WHEN after this acts like catch, not if
                elif token_upper == "EXCEPTION":
                    in_exception_block = True
                # Reset when we see END (closes the exception block or case expression)
                elif token_upper == "END":
                    in_exception_block = False
                    in_case_expression = False
                    saw_if_then = False  # Reset IF tracking when we see END

                # Track FOR and WHILE keywords
                if token_upper in ("FOR", "WHILE"):
                    saw_for_or_while = True

                # Handle "END IF", "END LOOP", etc.
                if (
                    last_nonwhitespace_token
                    and last_nonwhitespace_token.upper() == "END"
                ):
                    if token_upper in ("IF", "CASE", "LOOP", "WHILE", "FOR"):
                        # END IF, END CASE, END LOOP, END WHILE, END FOR
                        # All close control structures with synthetic {
                        # Emit "}nosync" to pop nesting without affecting br_count
                        # (br_count only tracks BEGIN/END pairs)
                        yield "}nosync"
                        last_nonwhitespace_token = None
                        pending_tokens = []
                        saw_for_or_while = False
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
                    # Normalize token for cognitive complexity
                    normalized = self._normalize_token(last_nonwhitespace_token)
                    yield normalized

                    # If we just emitted 'loop', emit synthetic '{'
                    # This applies to standalone LOOP, FOR LOOP, and WHILE LOOP
                    if last_nonwhitespace_token.upper() == "LOOP":
                        yield "{"
                        saw_for_or_while = False
                    # If we just emitted 'then' (from IF), emit synthetic '{'
                    # This pushes IF blocks onto the nesting stack
                    # But NOT for:
                    # - THEN in EXCEPTION WHEN (those act like catch clauses)
                    # - THEN in CASE WHEN (those act like switch cases)
                    # - THEN in ELSIF (only emit { once per IF...END IF structure)
                    elif (last_nonwhitespace_token.upper() == "THEN"
                          and not in_exception_block
                          and not in_case_expression
                          and not saw_if_then):
                        yield "{"
                        saw_if_then = True  # Mark that we've emitted { for this IF statement

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
            normalized = self._normalize_token(last_nonwhitespace_token)
            yield normalized
            # If we just emitted 'loop', emit synthetic '{'
            if last_nonwhitespace_token.upper() == "LOOP":
                yield "{"
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
        self.last_token = None  # Track last token to distinguish BEGIN's { from synthetic {

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
        """After trigger name, skip until DECLARE or BEGIN.
        Note: BEGIN is normalized to '{' by preprocessor for cognitive complexity.
        """
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
        elif token == "{":  # BEGIN normalized to { by preprocessor
            self.context.confirm_new_function()
            # Push function onto nesting stack and start tracking br_count
            self.context.add_bare_nesting()  # This pushes the pending FunctionInfo
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
        Note: BEGIN is normalized to '{' by preprocessor for cognitive complexity.
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
        elif token == "{":  # BEGIN normalized to { by preprocessor
            # Start of the implementation body
            # Push function onto nesting stack and start tracking br_count
            self.context.add_bare_nesting()  # This pushes the pending FunctionInfo
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
        Manually track BEGIN/END blocks (normalized to {/} by preprocessor).
        """
        token_lower = token.lower()
        token_upper = token.upper()

        # Handle nested procedure/function declarations
        if token_lower == "procedure":
            self.next(self._procedure_name)
            return
        elif token_lower == "function":
            self.next(self._function_name)
            return

        # Track FOR and WHILE to detect standalone LOOP
        if token_lower in ("for", "while"):
            self.last_control_keyword = token_upper

        # Handle LOOP keyword manually to avoid double-counting
        # - Standalone LOOP adds +1 complexity (not preceded by FOR/WHILE)
        # - LOOP after FOR/WHILE does not add complexity (FOR/WHILE already counted)
        elif token_lower == "loop":
            if self.last_control_keyword not in ("FOR", "WHILE"):
                # This is a standalone LOOP, add complexity
                self.context.add_condition()
            # Reset tracking after processing LOOP
            self.last_control_keyword = None

        # PL/SQL BEGIN/END and control structures normalized by preprocessor
        # "}" is from standalone "END" (closes BEGIN blocks, affects br_count)
        # "}nosync" is from "END IF/LOOP/CASE" (closes control structures, doesn't affect br_count)
        if token == "{":
            # Distinguish between { from BEGIN and synthetic { from THEN/LOOP
            # - Synthetic { appears after 'then' or 'loop' (emitted by preprocessor)
            # - { from BEGIN does not follow 'then' or 'loop'
            is_from_begin = (self.last_token and
                           self.last_token.lower() not in ('then', 'loop'))

            if self.br_count > 0:  # We're inside a function body
                if is_from_begin:
                    # Nested BEGIN block - increment br_count
                    self.br_count += 1
                self.context.add_bare_nesting()
        elif token == "}nosync":
            # This is from "END IF", "END LOOP", "END CASE" etc.
            # Pop nesting but do NOT decrement br_count (only BEGIN/END affect br_count)
            self.context.pop_nesting()
        elif token == "}":
            # This is from standalone END (closes BEGIN blocks)
            # Decrement br_count and pop nesting
            if self.br_count > 0:
                self.br_count -= 1
                if self.br_count == 0:
                    # This END closes the function/procedure
                    # Pop all remaining nested structures AND the FunctionInfo from the nesting stack
                    # Keep popping until we pop a FunctionInfo (which calls end_of_function)
                    from lizard import FunctionInfo
                    has_parent = len(self.context.stacked_functions) > 0
                    while self.context.nesting_stack:
                        popped = self.context.pop_nesting()
                        if isinstance(popped, FunctionInfo):
                            # Found and popped the function, we're done
                            break
                    if has_parent:
                        self.next(self._state_before_begin)
                    else:
                        self.next(self._state_global)
                    return
                else:
                    # This END closes a nested BEGIN block
                    self.context.pop_nesting()

        # Note: Basic conditions (if, elsif/elif, when, while, for, and, or)
        # are automatically counted by the condition_counter and cognitive_complexity_counter
        # processors based on the _conditions set in the Reader class.

        # Track last token for distinguishing BEGIN's { from synthetic {
        if token and not token.isspace():
            self.last_token = token
