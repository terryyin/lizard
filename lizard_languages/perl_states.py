"""Function and scope states for the Perl reader."""

from .code_reader import CodeStateMachine
from .perl_tokens import NAME_PREFIXES


class PerlStates(CodeStateMachine):
    def __init__(self, context):
        super(PerlStates, self).__init__(context)
        self.function_name = ''
        self.declaration_line = 0
        self.package_name = ''
        self.variable_name = ''
        self.brace_count = 0
        self.enclosing_brace_count = 0
        self.callback_paren_count = 0
        self.paren_count = 0
        self.in_attribute = False
        self.anonymous_count = 0
        self._state = self._state_global

    def _start_function(self, name, nested=False):
        if self.package_name:
            name = f"{self.package_name}::{name}"
        if nested:
            self.context.push_new_function(name)
        else:
            self.context.restart_new_function(name)

    def _is_sub_keyword(self, token):
        return token == 'sub' and self.last_token not in NAME_PREFIXES

    def _state_global(self, token):
        if token == 'package':
            self.next(self._state_package_dec)
        elif self._is_sub_keyword(token):
            self.function_name = ''
            self.declaration_line = self.context.current_line
            self.next(self._state_function_dec)
        elif token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
        elif token == '(':
            self.paren_count += 1
            self.next(self._state_function_call)
        elif token in ('$', 'my', 'our', 'local'):
            self.variable_name = ''
            self.next(self._state_variable)

    def _state_package_dec(self, token):
        if not token.isspace():
            self.package_name = token
            self.next(self._state_global)

    def _state_variable(self, token):
        if token == '$':
            # Skip the $ in variable name
            pass
        elif token == '=':
            self.next(self._state_assignment)
        elif token == ';':
            self.variable_name = ''
            self.next(self._state_global)
        elif not token.isspace() and self.variable_name == '':
            self.variable_name = token

    def _state_assignment(self, token):
        if self._is_sub_keyword(token):
            self.next(self._state_anon_sub)
        elif token == ';':
            self.variable_name = ''
            self.next(self._state_global)

    def _state_function_call(self, token):
        if self._is_sub_keyword(token):
            # Inline anonymous subroutine as argument
            self.anonymous_count += 1
            self._start_function('<anonymous>')
            self.next(self._state_anon_brace_search)
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.next(self._state_global)
        elif token == '(':
            self.paren_count += 1

    def _state_anon_sub(self, token):
        if token == '{':
            self.brace_count = 1
            name = '$' + self.variable_name if self.variable_name else '<anonymous>'
            self._start_function(name)
            self.next(self._state_function_body)

    def _state_function_dec(self, token):
        if token == '{':
            self.brace_count = 1
            if self.function_name:
                self._start_function(self.function_name)
                self.context.current_function.start_line = self.declaration_line
            self.next(self._state_function_body)
        elif token == ':':
            self.in_attribute = True
        elif token == ';':
            # Forward declaration like "sub func_name;"
            if self.function_name:
                self._start_function(self.function_name)
                self.context.current_function.start_line = self.declaration_line
                # Empty function body
                self.context.end_of_function()
            self.next(self._state_global)
        elif token == '(':
            # Function with parameter prototype like "sub fetch($)"
            self.paren_count = 1
            self.sub_state(self._state_function_prototype)
        elif self._is_sub_keyword(token):
            # Handle anonymous subroutine like 'callback(sub { ... })'
            self.anonymous_count += 1
            self._start_function('<anonymous>')
            self.next(self._state_anon_brace_search)
        elif not token.isspace():
            if not self.in_attribute:
                self.function_name = token
            else:
                # Skip attribute name
                self.in_attribute = False

    def _state_function_prototype(self, token):
        # Handle parameter prototypes after function name: sub fetch($) { ... }
        if token == ')':
            self.paren_count -= 1
            return self.paren_count == 0
        elif token == '(':
            self.paren_count += 1
        return False

    def _state_anon_brace_search(self, token):
        if token == '{':
            self.brace_count = 1
            self.next(self._state_function_body)
        elif token == '(':
            self.paren_count += 1
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.next(self._state_global)

    def _state_function_body(self, token):
        if token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 0:
                self.context.end_of_function()
                self.next(self._state_global)
        elif token == '?':
            # Ternary operator increases complexity
            self.context.add_condition()
        elif token == ':':
            # Colon part of ternary operator also increases complexity
            self.context.add_condition()
        elif self._is_sub_keyword(token):
            # Check if it's a nested named subroutine or anonymous
            self.next(self._state_nested_sub_dec)
        elif token == '(':
            # Track function calls inside function body
            self.paren_count += 1
            self.next(self._state_nested_call)

    def _state_nested_sub_dec(self, token):
        if token.isspace():
            return
        if token == '{':
            # Anonymous sub
            self.brace_count += 1
            self.anonymous_count += 1
            self.context.add_condition()  # Count sub as a condition
            self.next(self._state_function_body)
        elif token == '(':
            self.paren_count = 1
            self.sub_state(self._state_function_prototype)
        else:
            self._start_function(token, nested=True)
            self.next(self._state_nested_named_sub_brace_search)

    def _state_nested_named_sub_brace_search(self, token):
        if token == '{':
            self.enclosing_brace_count = self.brace_count
            self.brace_count = 1
            self.next(self._state_nested_sub_body)
        elif token.isspace():
            return
        elif token == ':':
            # Handle attributes in nested sub
            self.in_attribute = True
            return
        elif token == ';':
            # Forward declaration
            self.context.end_of_function()
            self.next(self._state_function_body)

    def _state_nested_sub_body(self, token):
        if token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 0:
                # End the nested function
                self.context.end_of_function()
                self.brace_count = self.enclosing_brace_count
                self.next(self._state_function_body)

    def _state_nested_call(self, token):
        if self._is_sub_keyword(token):
            # Inline anonymous subroutine as argument
            self.anonymous_count += 1
            self._start_function('<anonymous>', nested=True)
            self.callback_paren_count = self.paren_count
            self.next(self._state_nested_anon_search)
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.next(self._state_function_body)
        elif token == '(':
            self.paren_count += 1

    def _state_nested_anon_search(self, token):
        if token == '{':
            self.enclosing_brace_count = self.brace_count
            self.brace_count = 1
            self.next(self._state_nested_anon_body)
        elif token == '(':
            self.paren_count += 1
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.fold_into_enclosing_function()
                self.next(self._state_function_body)

    def _state_nested_anon_body(self, token):
        if token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 0:
                self.context.end_of_function()
                self.brace_count = self.enclosing_brace_count
                self.paren_count = self.callback_paren_count
                self.next(self._state_nested_call)
