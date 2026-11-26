'''
PHP function/class/match state machine.
'''

from .code_reader import CodeStateMachine

MEMBER_ACCESS = frozenset(('->', '?->', '::'))
_SKIPPED = frozenset(('fn', 'public', 'private', 'protected', 'static'))
_CLOSURE_EXPRESSION_CONTINUATION = frozenset((
    '(', '[', '->', '?->', '::', 'instanceof', '?', '?:', '??', '&&', '||',
    '==', '===', '!=', '!==', '<', '>', '<=', '>=', '%', '*', '**', '/', '+', '-', '.', '&', '|', '^',
))


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
        self.function_brace_level = 0
        self.started_function = False
        self.last_token = ''
        self.last_tokens = ''
        self.is_function_declaration = False
        self.assignment_name = ''
        self.assignment_parens = 0
        self.assigned_closure = None
        self.in_match = False
        self.match_case_count = 0

    def _state_global(self, token):
        if token in _SKIPPED or (self.last_token in MEMBER_ACCESS
                                 and token.isidentifier()):
            # An arrow function (PHP 7.4+), a visibility modifier, or a
            # member, `User::class` or `$app->use()`, starts no statement.
            pass
        elif token == 'use':
            # Enter use statement state
            self._state = self._state_use
        elif token == 'class':
            self._state = self._class_declaration
        elif token == 'trait':
            self._state = self._trait_declaration
        elif token == 'function':
            # Treat 'function' as function declaration
            self.is_function_declaration = True
            self._state = self._function_name
        elif token == 'match':
            self.in_match = True
            self.match_case_count = 0
            self.next(self._match_expression)
        elif token in ('if', 'switch', 'for', 'foreach', 'while', 'catch'):
            self.next(self._condition_expected)
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
            self.assignment_name = self.last_tokens
            self.assignment_parens = 0
            self._state = self._assignment_value
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
            else:
                self.last_tokens = ''

    def _assignment_value(self, token):
        if token == '\n' or token.startswith('#') or token.lower() == 'static':
            return
        if token == '(':
            self.assignment_parens += 1
            return
        if token != 'function':
            self.assignment_name = ''
        self._state = self._state_global
        self._state_global(token)

    def _state_use(self, token):
        """Handle use statements (use function, use const, etc.)"""
        if token == ';':
            # End of use statement, return to global state
            self._state = self._state_global
        # Ignore all other tokens in use statements

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
            self.assignment_name = ''
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
                self.function_name = self.assignment_name or "(anonymous)"
            self.bracket_level = 1
            self._state = self._function_args_continue
            self.context.push_new_function(self.function_name)
            if self.assignment_name == self.function_name:
                self.assigned_closure = self.context.current_function
            self.assignment_name = ''
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
            self._enter_function_body()
        elif token == ';':
            # Handle forward declarations in interface
            if self.started_function:
                self.context.end_of_function()
                self.started_function = False
            self._state = self._state_global

    def _function_body_or_return_type(self, token):
        if token == '{':
            self._enter_function_body()

    def _enter_function_body(self):
        self.function_brace_level = self.brace_level
        self.brace_level += 1
        self._state = self._function_body

    def _function_body(self, token):
        if token == '{':
            self.brace_level += 1
        elif token == '}':
            self.brace_level -= 1
            if self.brace_level == self.function_brace_level:
                # End of function
                if self.started_function:
                    self.context.end_of_function()
                    self.started_function = False
                self._state = self._finish_closure_assignment if self.assigned_closure else self._state_global

    def _finish_closure_assignment(self, token):
        if token == '\n' or token.startswith('#'):
            return
        if self.assignment_parens and token == ')':
            self.assignment_parens -= 1
            return
        # "and", "or" and "xor" have lower precedence than assignment.
        if self.assignment_parens or token.lower() in _CLOSURE_EXPRESSION_CONTINUATION:
            # The assignment receives the expression result.
            closure = self.assigned_closure
            closure.long_name = '(anonymous)' + closure.long_name[len(closure.name):]
            closure.name = '(anonymous)'
        self.assigned_closure = None
        self._state = self._state_global
        self._state_global(token)

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
