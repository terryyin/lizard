'''
Language parser for R
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


class RReader(CodeReader, ScriptLanguageMixIn):
    """R language reader for parsing R code and calculating complexity metrics."""

    ext = ['r', 'R']
    language_names = ['r', 'R']

    # R-specific conditions that increase cyclomatic complexity
    _conditions = {
        'if', 'else if', 'for', 'while', 'repeat', 'switch',
        '&&', '||', '&', '|', 'ifelse',
        'tryCatch', 'try'
    }

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
        # R-specific token patterns
        r_patterns = (
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


class RStates(CodeStateMachine):
    """State machine for parsing R function definitions and complexity."""

    def __init__(self, context):
        super(RStates, self).__init__(context)
        self.recent_tokens = []  # Track recent tokens to find function names
        self.brace_count = 0  # Track brace nesting for function bodies
        self.in_braced_function = False  # Track if current function uses braces
        self.additional_function_names = []  # Store additional names for multiple assignment

    def _state_global(self, token):
        """Global state - looking for function definitions."""
        # Track recent non-whitespace tokens
        if not token.isspace() and token != '\n':
            self.recent_tokens.append(token)
            if len(self.recent_tokens) > 10:  # Keep only last 10 tokens
                self.recent_tokens.pop(0)

        # Look for function keyword after assignment operators
        if token == 'function':
            # Check if we have recent tokens: [name, assignment_op, 'function']
            if len(self.recent_tokens) >= 2:
                # recent_tokens now contains [..., assignment_op, 'function']
                assignment_op = self.recent_tokens[-2]  # The token before 'function'
                if assignment_op in ['<-', '=']:
                    # Handle multiple assignments by creating separate functions
                    func_names = self._extract_function_names()

                    # Create the first function (this will be the main one with the function body)
                    self._start_function(func_names[0])
                    self._state = self._function_params

                    # Store additional names for later processing
                    self.additional_function_names = func_names[1:] if len(func_names) > 1 else []
                    return

            # If we get here, it's an anonymous function or not a proper assignment
            self._start_function("(anonymous)")
            self._state = self._function_params

    def _extract_function_names(self):
        """Extract all function names from recent tokens, handling multiple assignments."""
        if len(self.recent_tokens) < 3:
            return ["(anonymous)"]

        # Look backwards from the assignment operator to find all function names
        # For multiple assignment like: a <- b <- c <- function(...)
        # recent_tokens ends with [..., 'a', '<-', 'b', '<-', 'c', '<-', 'function']
        assignment_index = len(self.recent_tokens) - 2  # Position of assignment operator

        function_names = []
        i = assignment_index - 1  # Start from token before assignment operator
        current_name_tokens = []

        while i >= 0:
            token = self.recent_tokens[i]

            # If we hit an assignment operator, we've found a complete variable name
            if token in ['<-', '=']:
                if current_name_tokens:
                    function_names.append(''.join(reversed(current_name_tokens)))
                    current_name_tokens = []
                i -= 1
                continue

            # Stop if we hit keywords or operators that shouldn't be part of function names
            if token in ['function', '(', ')', '{', '}', '\n']:
                break

            # Valid R identifier characters and dots
            if token.replace('_', 'a').replace('.', 'a').isalnum() or token == '.':
                current_name_tokens.append(token)
                i -= 1
            else:
                break

        # Add the last name if we have one
        if current_name_tokens:
            function_names.append(''.join(reversed(current_name_tokens)))

        # Return names in the correct order (left to right as they appear in code)
        return list(reversed(function_names)) if function_names else ["(anonymous)"]

    def _extract_function_name(self):
        """Extract the first function name (for backward compatibility)."""
        names = self._extract_function_names()
        return names[0] if names else "(anonymous)"

    def _start_function(self, name):
        """Start tracking a new function."""
        self.context.restart_new_function(name)

    def _function_params(self, token):
        """Expecting function parameters."""
        if token == '(':
            self.context.add_to_long_function_name("(")
            self._state = self._read_params
        else:
            # Single expression function without parentheses - rare in R
            self._state = self._function_body
            self._function_body(token)

    def _read_params(self, token):
        """Read function parameters until closing parenthesis."""
        if token == ')':
            self.context.add_to_long_function_name(")")
            self._state = self._function_body
        elif token not in ['\n'] and not token.isspace():
            self.context.parameter(token)
            if token != '(':
                self.context.add_to_long_function_name(" " + token)

    def _function_body(self, token):
        """In function body - track complexity and nested functions."""
        # Note: Complexity conditions are automatically counted by the framework
        # based on reader.conditions, so we don't need to manually count them here

        # Continue tracking tokens even in function body for nested function detection
        if not token.isspace() and token != '\n':
            self.recent_tokens.append(token)
            if len(self.recent_tokens) > 10:  # Keep only last 10 tokens
                self.recent_tokens.pop(0)

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
            # Check if this is a nested function assignment
            if len(self.recent_tokens) >= 2:
                assignment_op = self.recent_tokens[-2]  # The token before 'function'
                if assignment_op in ['<-', '=']:
                    # End current function first
                    self.context.end_of_function()

                    # Handle multiple assignments for nested functions too
                    func_names = self._extract_function_names()

                    # Start a new function for the nested function
                    self._start_function(func_names[0])
                    self._state = self._function_params
                    # Reset brace counting for the new function
                    self.brace_count = 0
                    self.in_braced_function = False

                    # Store additional names for later processing
                    self.additional_function_names = func_names[1:] if len(func_names) > 1 else []
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
        if token.replace('_', 'a').replace('.', 'a').isalnum() or token == '.':
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
