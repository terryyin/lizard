'''
Language parser for R
'''

from .code_reader import CodeReader, CodeStateMachine
from .r_declaration import RDeclarationMixin, is_r_identifier_token
from .script_language import ScriptLanguageMixIn


class RReader(CodeReader, ScriptLanguageMixIn):
    """R language reader for parsing R code and calculating complexity metrics."""

    ext = ['r', 'R']
    language_names = ['r', 'R']

    # Separated condition categories
    _control_flow_keywords = {'if', 'else if', 'for', 'while', 'repeat', 'switch',
                              'tryCatch', 'try', 'ifelse'}  # ifelse is a vectorized control function
    # R has both short-circuit (&&, ||) and element-wise (&, |) operators
    # Both types count toward CCN as they represent conditional logic (vectorized or not)
    # Users can use -Enonstrict to exclude logical operators if desired
    _logical_operators = {'&&', '||', '&', '|'}
    _case_keywords = set()
    _ternary_operators = set()

    def __init__(self, context):
        super(RReader, self).__init__(context)
        self.parallel_states = [RStates(context)]

    def preprocess(self, tokens):
        """Preprocess tokens - for now just pass them through."""
        for token in tokens:
            yield token

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        """Generate tokens for R code with R-specific patterns."""
        # R-specific token patterns. The raw-string alternative is one token
        # and must precede generic strings, which stop at the first quote.
        r_patterns = (
            r"""|[rR](?P<rq>["'])(?P<rd>-*)(?:\(.*?\)|\[.*?\]|\{.*?\}|\|.*?\|)(?P=rd)(?P=rq)"""
            r"|`(?:\\.|[^`\\])*`"  # Backtick-quoted identifiers
            r"|<-"          # Assignment operator <-
            r"|->"          # Assignment operator ->
            r"|%[a-zA-Z_*/>]+%"  # Special operators like %in%, %*%, %>%, %/%, etc.
            r"|\.\.\."      # Ellipsis for variable arguments
            r"|:::"         # Internal namespace operator (must come before ::)
            r"|::"          # Namespace operator
        )

        return ScriptLanguageMixIn.generate_common_tokens(
            source_code,
            r_patterns + addition,
            token_class
        )


class RStates(RDeclarationMixin, CodeStateMachine):
    """State machine for parsing R function definitions and complexity."""

    def __init__(self, context):
        super(RStates, self).__init__(context)
        self.recent_tokens = []  # Track recent tokens to find function names
        self.brace_count = 0  # Track brace nesting for function bodies
        self.in_braced_function = False  # Track if current function uses braces
        self.additional_function_names = []  # Store additional names for multiple assignment
        self._parameter_depth = 0
        self._parameter_tokens = []

    def _state_global(self, token):
        """Global state - looking for function definitions."""
        self._record_recent_token(token)

        if token == 'function':
            self._start_declaration(allow_anonymous=True)

    def _end_enclosing_for_nested_declaration(self):
        """Close the enclosing function before entering a nested declaration."""
        self.context.end_of_function()
        self.brace_count = 0
        self.in_braced_function = False

    def _function_body(self, token):
        """In function body - track complexity and nested functions."""
        # Note: Complexity conditions are automatically counted by the framework
        # based on reader.conditions, so we don't need to manually count them here

        # Continue tracking tokens even in function body for nested function detection
        self._record_recent_token(token)

        # Track braces
        if token == '{':
            if self.brace_count == 0:
                self.in_braced_function = True
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 0 and self.in_braced_function:
                # End of braced function
                self._end_current_function()
                return

        # Handle nested functions - treat them as separate functions
        if token == 'function':
            if self._start_declaration(
                allow_anonymous=False,
                before_start=self._end_enclosing_for_nested_declaration,
            ):
                return

        # For single-line functions without braces, end at newline
        elif token == '\n' and not self.in_braced_function:
            self._end_current_function()

    def _end_current_function(self):
        """End the current function and reset state."""
        # Check if this might be a right assignment case
        # We need to temporarily not end the function to see if there's a right assignment
        self._state = self._check_right_assignment
        self.brace_count = 0
        self.in_braced_function = False

    def _check_right_assignment(self, token):
        """Check if there's a right assignment after function end."""
        # Skip whitespace and comments
        if token.isspace() or token == '\n' or token.startswith('#'):
            return

        # Look for right assignment operator
        if token == '->':
            self._state = self._read_right_assignment_name
            return

        # If we encounter anything else, this is not a right assignment
        # End the function and create additional functions for multiple assignments
        self._finalize_function_with_multiple_assignments()
        self._state = self._state_global
        self._state_global(token)

    def _finalize_function_with_multiple_assignments(self):
        """End the current function and create additional functions for multiple assignments."""
        # Get the current function's information before ending it
        current_func = self.context.current_function

        # End the current function
        self.context.end_of_function()

        # Create additional function entries for multiple assignments
        if self.additional_function_names and current_func:
            for func_name in self.additional_function_names:
                # Create a new function with the same complexity and line info
                self.context.restart_new_function(func_name)
                # Copy the complexity from the original function
                if hasattr(current_func, 'cyclomatic_complexity'):
                    self.context.current_function.cyclomatic_complexity = current_func.cyclomatic_complexity
                # Set the same line range
                self.context.current_function.start_line = current_func.start_line
                self.context.current_function.end_line = current_func.end_line
                # End this function immediately
                self.context.end_of_function()

        # Clear the additional names
        self.additional_function_names = []

    def _read_right_assignment_name(self, token):
        """Read the function name after right assignment operator."""
        # Skip whitespace
        if token.isspace() or token == '\n':
            return

        # This should be the function name
        if is_r_identifier_token(token):
            # Update the current function's name
            if self.context.current_function:
                self.context.current_function.name = token

            # End the function and create additional functions for multiple assignments
            self._finalize_function_with_multiple_assignments()
            self._state = self._state_global
            return

        # If we get something unexpected, treat as anonymous function
        self._finalize_function_with_multiple_assignments()
        self._state = self._state_global
        self._state_global(token)

    def statemachine_before_return(self):
        """Called when processing is complete - end any open functions."""
        if self._state in [self._function_body, self._check_right_assignment, self._read_right_assignment_name]:
            # End any open function and process multiple assignments
            if hasattr(self.context, 'current_function') and self.context.current_function:
                self._finalize_function_with_multiple_assignments()
