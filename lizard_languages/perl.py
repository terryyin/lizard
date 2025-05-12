'''
Language parser for Perl
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


class PerlCommentsMixin(object):
    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('#'):
            return token  # Return the entire comment including #
        return None


class PerlReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['pl', 'pm']
    language_names = ['perl']
    _conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach',
                       '&&', '||', '?', ':', 'when', 'given', 'default', 'do'])

    def __init__(self, context):
        super(PerlReader, self).__init__(context)
        self.parallel_states = [PerlStates(context)]

    def preprocess(self, tokens):
        comment = None
        for token in tokens:
            if comment is not None:
                if token == '\n':
                    yield comment
                    comment = None
                    yield token
                else:
                    comment += token
            elif token == '#':
                comment = token
            else:
                yield token
        if comment is not None:
            yield comment

    def _state(self, token):
        current_state = self.parallel_states[-1]
        if token == '\n':
            return
        return current_state.state(token)

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('#'):
            # For forgiveness comments, return the entire comment
            stripped = token.lstrip('#').strip()
            if stripped.startswith('lizard forgives') or stripped.startswith('#lizard forgives'):
                return '#lizard forgives'  # Return standardized forgiveness comment
            return stripped  # Return the stripped comment for other case
        return None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return ScriptLanguageMixIn.generate_common_tokens(source_code, addition, token_class)


class PerlStates(CodeStateMachine):
    _conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach',
                       '&&', '||', '?', ':', 'when', 'given', 'default', 'do'])

    def __init__(self, context):
        super(PerlStates, self).__init__(context)
        self.function_name = ''
        self.package_name = ''
        self.variable_name = ''
        self.brace_count = 0
        self.paren_count = 0
        self.in_attribute = False
        self.anonymous_count = 0
        self._state = self._state_global

    def _state_global(self, token):
        if token == 'package':
            self.next(self._state_package_dec)
        elif token == 'sub':
            self.function_name = ''
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
        if token == 'sub':
            self.next(self._state_anon_sub)
        elif token == ';':
            self.variable_name = ''
            self.next(self._state_global)

    def _state_function_call(self, token):
        if token == 'sub':
            # Inline anonymous subroutine as argument
            self.anonymous_count += 1
            full_name = "<anonymous>"
            if self.package_name:
                full_name = f"{self.package_name}::{full_name}"

            self.context.try_new_function(full_name)
            self.context.confirm_new_function()
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
            full_name = '<anonymous>'
            # Use variable name if available for more readable function name
            if self.variable_name:
                full_name = '$' + self.variable_name

            if self.package_name:
                full_name = f"{self.package_name}::{full_name}"

            self.context.try_new_function(full_name)
            self.context.confirm_new_function()
            self.next(self._state_function_body)

    def _state_function_dec(self, token):
        if token == '{':
            self.brace_count = 1
            if self.function_name:
                full_name = self.function_name
                if self.package_name:
                    full_name = f"{self.package_name}::{self.function_name}"
                self.context.try_new_function(full_name)
                self.context.confirm_new_function()
            self.next(self._state_function_body)
        elif token == ':':
            self.in_attribute = True
        elif token == ';':
            # Forward declaration like "sub func_name;"
            if self.function_name:
                full_name = self.function_name
                if self.package_name:
                    full_name = f"{self.package_name}::{self.function_name}"
                self.context.try_new_function(full_name)
                self.context.confirm_new_function()
                # Empty function body
                self.context.end_of_function()
            self.next(self._state_global)
        elif token == '(':
            # Function with parameter prototype like "sub fetch($)"
            # Save the current function name and look for closing paren
            self.paren_count = 1
            self.next(self._state_function_prototype)
        elif token == 'sub':
            # Handle anonymous subroutine like 'callback(sub { ... })'
            self.anonymous_count += 1
            full_name = "<anonymous>"
            if self.package_name:
                full_name = f"{self.package_name}::{full_name}"
            self.context.try_new_function(full_name)
            self.context.confirm_new_function()
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
            if self.paren_count == 0:
                # Return to function declaration state to handle the opening brace
                self.next(self._state_function_dec)
        elif token == '(':
            self.paren_count += 1

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
        elif token == 'sub':
            # Check if it's a nested named subroutine or anonymou
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
            anon_name = "<anonymous>"
            if self.package_name:
                anon_name = f"{self.package_name}::{anon_name}"
            self.context.add_condition()  # Count sub as a condition
            self.next(self._state_function_body)
        else:
            # Named nested sub
            nested_func_name = token
            full_name = nested_func_name
            if self.package_name:
                full_name = f"{self.package_name}::{nested_func_name}"

            # Create a new function for the nested sub
            self.context.try_new_function(full_name)
            self.context.confirm_new_function()
            self.next(self._state_nested_named_sub_brace_search)

    def _state_nested_named_sub_brace_search(self, token):
        if token == '{':
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
                # Return to parent function
                self.next(self._state_function_body)

    def _state_nested_call(self, token):
        if token == 'sub':
            # Inline anonymous subroutine as argument
            self.anonymous_count += 1
            full_name = "<anonymous>"
            if self.package_name:
                full_name = f"{self.package_name}::{full_name}"

            self.context.try_new_function(full_name)
            self.context.confirm_new_function()
            self.next(self._state_nested_anon_search)
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.next(self._state_function_body)
        elif token == '(':
            self.paren_count += 1

    def _state_nested_anon_search(self, token):
        if token == '{':
            self.brace_count += 1
            self.next(self._state_nested_anon_body)
        elif token == '(':
            self.paren_count += 1
        elif token == ')':
            self.paren_count -= 1
            if self.paren_count == 0:
                self.next(self._state_function_body)

    def _state_nested_anon_body(self, token):
        if token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 1:  # Back to outer function level
                self.context.end_of_function()
                self.next(self._state_function_body)
