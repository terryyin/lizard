'''
Language parser for PHP
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin


class PHPLanguageStates(CodeStateMachine):
    """
    PHP-specific state machine that properly handles modern PHP syntax
    including classes, visibility modifiers and return types.
    """

    def __init__(self, context):
        super(PHPLanguageStates, self).__init__(context)
        self.function_name = ''
        self.class_name = None
        self.trait_name = None
        self.in_class = False
        self.in_trait = False
        self.bracket_level = 0
        self.brace_level = 0
        self.started_function = False
        self.last_token = ''
        self.last_tokens = ''
        self.is_function_declaration = False
        self.assignments = []
        self.in_match = False
        self.match_case_count = 0

    def _state_global(self, token):
        if token == 'class':
            self._state = self._class_declaration
        elif token == 'trait':
            self._state = self._trait_declaration
        elif token == 'function':
            self.is_function_declaration = True
            self._state = self._function_name
        elif token == 'fn':
            # Skip arrow functions (PHP 7.4+), don't treat them as full functions
            pass
        elif token == 'match':
            self.in_match = True
            self.match_case_count = 0
            self.next(self._match_expression)
        elif token in ('if', 'switch', 'for', 'foreach', 'while', 'catch'):
            self.next(self._condition_expected)
        elif token in ('public', 'private', 'protected', 'static'):
            # Skip visibility modifiers
            pass
        elif token == '=>' and self.in_match:
            # Count each case in a match expression
            self.match_case_count += 1
        elif token == '}' and self.in_match:
            # End of match expression
            self.in_match = False
            # Add the match cases to complexity (subtract 1 because the 'match' keyword is already counted)
            if self.match_case_count > 0:
                for _ in range(self.match_case_count - 1):
                    self.context.add_condition()
            self.match_case_count = 0
        elif token == '=':
            # Handle function assignment
            self.function_name = self.last_tokens.strip()
            self.assignments.append(self.function_name)
        elif token == '{':
            self.brace_level += 1
        elif token == '}':
            self.brace_level -= 1
            if self.brace_level == 0:
                if self.in_class:
                    self.in_class = False
                    self.class_name = None
                if self.in_trait:
                    self.in_trait = False
                    self.trait_name = None

        # Update tokens
        self.last_token = token
        if token not in [' ', '\t', '\n']:
            if token not in ['=', ';', '{', '}', '(', ')', ',']:
                self.last_tokens = token
            elif token == '=' and self.last_tokens:
                # Keep the last tokens for assignment
                pass
            else:
                self.last_tokens = ''

    def _trait_declaration(self, token):
        if token and not token.isspace() and token not in ['{', '(']:
            self.trait_name = token
            self.in_trait = True
            self._state = self._state_global
        elif token == '{':
            self.brace_level += 1
            self._state = self._state_global

    def _class_declaration(self, token):
        if token and not token.isspace() and token not in ['{', '(', 'extends', 'implements']:
            self.class_name = token
            self.in_class = True
            self._state = self._state_global
        elif token == '{':
            self.brace_level += 1
            self._state = self._state_global

    def _function_name(self, token):
        if token and not token.isspace() and token != '(':
            method_name = token
            if self.in_class and self.class_name:
                # In class, use ClassName::methodName format
                long_name = f"{self.class_name}::{method_name}"
                short_name = method_name  # Store original name for compatibility with old tests
            elif self.in_trait and self.trait_name:
                # In trait, use TraitName::methodName format
                long_name = f"{self.trait_name}::{method_name}"
                short_name = method_name
            else:
                long_name = method_name
                short_name = method_name
            self.function_name = long_name
            self._state = self._function_args
            # Store original name for backward compatibility
            self.short_function_name = short_name
        elif token == '(':
            # Anonymous function
            if self.in_class:
                self.function_name = f"{self.class_name}::(anonymous)"
            elif self.in_trait:
                self.function_name = f"{self.trait_name}::(anonymous)"
            else:
                if self.assignments and self.assignments[-1]:
                    self.function_name = self.assignments[-1]
                    self.assignments.pop()
                else:
                    self.function_name = "(anonymous)"
            self.bracket_level = 1
            self._state = self._function_args_continue
            self.context.push_new_function(self.function_name)
            self.started_function = True

    def _function_args(self, token):
        if token == '(':
            self.bracket_level = 1
            # Compatibility handling for old tests
            if self.in_class and self.class_name and not self.is_function_declaration:
                self.context.push_new_function(self.short_function_name)
            else:
                self.context.push_new_function(self.function_name)
            self.started_function = True
            self._state = self._function_args_continue

    def _function_args_continue(self, token):
        if token == '(':
            self.bracket_level += 1
        elif token == ')':
            self.bracket_level -= 1
            if self.bracket_level == 0:
                self._state = self._function_return_type_or_body
        elif token.startswith('$'):
            # Found a parameter (PHP parameters start with $)
            if self.started_function:
                # Make sure we count each parameter uniquely
                self.context.add_to_long_function_name(" " + token)
                self.context.parameter(token)

    def _function_return_type_or_body(self, token):
        if token == ':':
            # Skip return type declaration
            self._state = self._function_body_or_return_type
        elif token == '{':
            # Function body starts
            self.brace_level += 1
            self._state = self._function_body
        elif token == ';':
            # Handle forward declarations in interface
            if self.started_function:
                self.context.end_of_function()
                self.started_function = False
            self._state = self._state_global

    def _function_body_or_return_type(self, token):
        if token == '{':
            # Found the function body opening after return type
            self.brace_level += 1
            self._state = self._function_body

    def _function_body(self, token):
        if token == '{':
            self.brace_level += 1
        elif token == '}':
            self.brace_level -= 1
            if self.brace_level == self.in_class:  # Using in_class as boolean (0/1)
                # End of function
                if self.started_function:
                    self.context.end_of_function()
                    self.started_function = False
                self._state = self._state_global

    def _condition_expected(self, token):
        if token == '(':
            self.bracket_level = 1
            self._state = self._condition_continue

    def _condition_continue(self, token):
        if token == '(':
            self.bracket_level += 1
        elif token == ')':
            self.bracket_level -= 1
            if self.bracket_level == 0:
                self._state = self._state_global

    def _match_expression(self, token):
        if token == '(':
            self.bracket_level = 1
            self._state = self._match_expression_continue

    def _match_expression_continue(self, token):
        if token == '(':
            self.bracket_level += 1
        elif token == ')':
            self.bracket_level -= 1
            if self.bracket_level == 0:
                self._state = self._state_global


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']
    _conditions = set(['if', 'elseif', 'for', 'foreach', 'while', '&&', '||', '?',
                       'catch', 'case', 'match'])

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition += r"|(?:\$\w+)"
        addition += r"|(?:\<{3}(?P<quote>\w+).*?(?P=quote))"
        current_pos = 0
        code_block_pattern = re.compile(
                r"\<\?(?:php)?(.*?)(?:(\?\>)|\Z)",
                re.M | re.S)
        for match in code_block_pattern.finditer(source_code):
            if source_code[current_pos:match.start()]:
                yield '"' + source_code[current_pos:match.start()] + '"'
            for token in CodeReader.generate_tokens(
                    match.group(1), addition, token_class):
                yield token
            current_pos = match.end()
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [PHPLanguageStates(context)]
