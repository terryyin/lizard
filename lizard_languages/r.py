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
        'if', 'for', 'while', 'repeat', 'switch', 
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
                    # Find function name (should be before the assignment operator)
                    if len(self.recent_tokens) >= 3:
                        func_name = self.recent_tokens[-3]  # The token before assignment
                    else:
                        func_name = "(anonymous)"
                    self._start_function(func_name)
                    self._state = self._function_params
                    return
            
            # If we get here, it's an anonymous function or not a proper assignment
            self._start_function("(anonymous)")
            self._state = self._function_params
                
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
                
        # Handle nested functions - for now, ignore them to keep main function detection simple
        # This is a simplification - full R support would need proper nested function handling
        if token == 'function':
            # Just ignore nested function keywords - they don't affect the main function's complexity
            pass
            
        # For single-line functions without braces, end at newline
        elif token == '\n' and not self.in_braced_function:
            self._end_current_function()
            
    def _end_current_function(self):
        """End the current function and reset state."""
        self.context.end_of_function()
        self.brace_count = 0
        self.in_braced_function = False
        self._state = self._state_global
        
    def statemachine_before_return(self):
        """Called when processing is complete - end any open functions."""
        if self._state == self._function_body:
            self._end_current_function()
