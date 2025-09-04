'''
Language parser for R
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


class RReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['r', 'R']
    language_names = ['r', 'rlang']

    _conditions = set(['if', 'for', 'while', 'repeat', '&&', '||', '?'])

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Recognize R assignment operator '<-'
        addition = addition + r"|<-"
        return ScriptLanguageMixIn.generate_common_tokens(
            source_code,
            addition,
            token_class)

    def __init__(self, context):
        super(RReader, self).__init__(context)
        self.parallel_states = [RStates(context)]


class RStates(CodeStateMachine):
    # Simple state machine for R functions
    def __init__(self, context):
        super(RStates, self).__init__(context)
        self.pending_name = None
        self.pending_name_stack = []
        self.brace_depth = 0
        self.brace_stack = []
        self.name_buffer = ''

    def _is_identifier(self, token):
        return bool(token) and (token[0].isalpha() or token[0] in '_.') and token not in ('function',)

    def _accumulate_name(self, token):
        if token == '.':
            if self.name_buffer and self.name_buffer[-1] != '.':
                self.name_buffer += '.'
        elif self._is_identifier(token):
            self.name_buffer += token
        elif token.strip() == '':
            pass
        elif token in ('<-', '='):
            pass
        else:
            self.name_buffer = ''

    def _begin_function_wait_lparen(self):
        # Enter waiting for '(' of a new function
        self._state = self._wait_lparen
        # Save outer pending name context for restoration after this function
        self.pending_name_stack.append(self.pending_name)

    def _finish_function(self):
        # Restore outer name context
        self.pending_name = self.pending_name_stack.pop() if self.pending_name_stack else None
        # Restore outer brace depth and state
        if self.brace_stack:
            self.brace_depth = self.brace_stack.pop()
            self._state = self._in_body if self.brace_depth is not None else self._state_global
        else:
            self.brace_depth = 0
            self._state = self._state_global

    def _state_global(self, token):
        self._accumulate_name(token)
        if token in ('<-', '='):
            self.pending_name = self.name_buffer or self.last_token
            self.name_buffer = ''
        elif token == 'function':
            self._begin_function_wait_lparen()
        elif token == '{':
            self.context.add_bare_nesting()
        elif token == '}':
            self.context.pop_nesting()

    def _wait_lparen(self, token):
        if token == '(':
            # Create function now so parameters attach to it
            name = self.pending_name if self.pending_name else '(anonymous)'
            self.pending_name = None
            self.context.push_new_function(name)
            self._state = self._params
        # else: ignore until '('

    def _params(self, token):
        if token == ')':
            self._state = self._body_wait
        else:
            # Pass through all tokens to parameter builder
            self.context.parameter(token)

    def _body_wait(self, token):
        if token == '{':
            # Push outer brace depth and enter body
            self.brace_stack.append(self.brace_depth)
            self.brace_depth = 1
            self._state = self._in_body
        elif token != '\n' and not token.isspace():
            # One-line function without braces: treat as body with depth 0
            self.brace_stack.append(self.brace_depth)
            self.brace_depth = 0
            self._state = self._in_body
            self._in_body(token)

    def _in_body(self, token):
        self._accumulate_name(token)
        if token in ('<-', '='):
            self.pending_name = self.name_buffer or self.last_token
            self.name_buffer = ''
            return
        # End for non-braced expression body
        if self.brace_depth == 0 and token in (',', ')', '}', '\n'):
            self.context.end_of_function()
            self._finish_function()
            # Re-handle current token in restored state
            self._state(token)
            return
        if token == 'function':
            # Nested function inside body
            self._begin_function_wait_lparen()
            return
        if token == '{':
            self.brace_depth += 1
        elif token == '}':
            if self.brace_depth > 0:
                self.brace_depth -= 1
            if self.brace_depth == 0:
                self.context.end_of_function()
                self._finish_function()