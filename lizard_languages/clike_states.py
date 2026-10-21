"""Function recognition shared by C-like language readers."""

from .code_reader import CodeStateMachine


# pylint: disable=R0903
class CLikeStates(CodeStateMachine):
    ''' This is the reader for C, C++ and Java. '''
    parameter_bracket_open = '(<'
    parameter_bracket_close = ')>'

    def __init__(self, context):
        super(CLikeStates, self).__init__(context)
        self.bracket_stack = []
        self._saved_tokens = []

    def try_new_function(self, name):
        self.context.try_new_function(name)
        self._state = self._state_function
        if name == 'operator':
            self._state = self._state_operator

    def _state_global(self, token):
        if token[0].isalpha() or token[0] in '_~':
            self.try_new_function(token)
        elif token == '[':
            # Check if this might be a lambda expression (C++ only)
            # Java doesn't have lambda expressions, so skip lambda detection for Java
            if not hasattr(self, 'class_name'):  # JavaStates has class_name attribute
                self._state = self._state_lambda_check

    def _state_function(self, token):
        if token == '(':
            self.next(self._state_dec, token)
        elif token == '::':
            self.context.add_to_function_name(token)
            self.next(self._state_name_with_space)
        elif token == '<':
            self.next(self._state_template_in_name, token)
        else:
            self.next(self._state_global, token)

    @CodeStateMachine.read_inside_brackets_then("<>", "_state_function")
    def _state_template_in_name(self, token):
        self.context.add_to_function_name(token)

    def _state_operator(self, token):
        if token != '(':
            self._state = self._state_operator_next
        self.context.add_to_function_name(' ' + token)

    def _state_operator_next(self, token):
        if token == '(':
            self._state_function(token)
        else:
            self.context.add_to_function_name(' ' + token)

    def _state_name_with_space(self, token):
        self._state = self._state_operator \
            if token == 'operator' else self._state_function
        self.context.add_to_function_name(token)

    @CodeStateMachine.read_inside_brackets_then("()", "_state_dec_to_imp")
    def _state_dec(self, token):
        if token in self.parameter_bracket_open:
            self.bracket_stack.append(token)
        elif token in self.parameter_bracket_close:
            if self.bracket_stack:
                self.bracket_stack.pop()
            else:
                self.next(self._state_global)
        elif len(self.bracket_stack) == 1:
            if token != 'void':  # void is a reserved keyword, meaning no parameters
                self.context.parameter(token)
            return
        self.context.add_to_long_function_name(token)

    def _state_dec_to_imp(self, token):
        if token in ('const', '&', '&&'):
            self.context.add_to_long_function_name(" " + token)
        elif token == 'throw':
            self._state = self._state_throw
        elif token == 'throws':
            self._state = self._state_throws
        elif token == '->':
            self._state = self._state_trailing_return
        elif token == 'noexcept':
            self._state = self._state_noexcept
        elif token == '(':
            long_name = self.context.current_function.long_name
            self.try_new_function(long_name)
            self._state_function(token)
        elif token == '{':
            self.next(self._state_entering_imp, "{")
        elif token == ":":
            self._state = self._state_initialization_list
        elif token == "[":
            self._state = self._state_attribute
            self._state(token)
        elif not (token[0].isalpha() or token[0] == '_'):
            self._state = self._state_global
            self._state(token)
        else:
            self._state = self._state_old_c_params
            self._saved_tokens = [token]

    @CodeStateMachine.read_inside_brackets_then("()")
    def _state_throw(self, _):
        self._state = self._state_dec_to_imp

    @CodeStateMachine.read_until_then(';{')
    def _state_throws(self, token, _):
        self._state = self._state_dec_to_imp
        self._state(token)

    def _state_noexcept(self, token):
        if token == '(':
            self._state = self._state_throw
        else:
            self._state = self._state_dec_to_imp
        self._state(token)

    @CodeStateMachine.read_until_then(';{')
    def _state_trailing_return(self, token, _):
        self._state = self._state_dec_to_imp
        self._state(token)

    def _state_old_c_params(self, token):
        self._saved_tokens.append(token)
        if token == ';':
            self._saved_tokens = []
            self._state = self._state_dec_to_imp
        elif token == '{':
            if len(self._saved_tokens) == 2:
                self._saved_tokens = []
                self._state_dec_to_imp(token)
                return
            self._state = self._state_global
            for tkn in self._saved_tokens:
                self._state(tkn)
        elif token == '(':
            self._state = self._state_global
            for tkn in self._saved_tokens:
                self._state(tkn)

    def _state_initialization_list(self, token):
        self._state = self._state_one_initialization
        if token == '{':
            self.next(self._state_entering_imp, "{")

    @CodeStateMachine.read_until_then('({')
    def _state_one_initialization(self, token, _):
        if token == "(":
            self._state = self._state_initialization_value1
        else:
            self._state = self._state_initialization_value2
        self._state(token)

    @CodeStateMachine.read_inside_brackets_then("()")
    def _state_initialization_value1(self, _):
        self._state = self._state_initialization_list

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _state_initialization_value2(self, _):
        self._state = self._state_initialization_list

    def _state_entering_imp(self, token):
        self.context.confirm_new_function()
        self.next(self._state_imp, token)

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _state_imp(self, _):
        self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("[]", "_state_dec_to_imp")
    def _state_attribute(self, _):
        "Ignores function attributes with C++11 syntax, i.e., [[ attribute ]]."
        pass

    def _state_lambda_check(self, token):
        """Check if this is a lambda expression or a function attribute."""
        if token == ']':
            # This is an empty capture list [](params)
            self._state = self._state_lambda_params
        elif token == '[':
            # This is a function attribute [[attribute]]
            self._state = self._state_attribute
        else:
            # This is a lambda with capture list [capture](params)
            self._state = self._state_lambda_capture

    def _state_lambda_params(self, token):
        """Handle lambda parameters and body."""
        if token == '(':
            # Start of parameter list
            self.bracket_stack.append('(')
            self._state = self._state_lambda_param_list
        else:
            # No parameters, check for body or go back to global
            self._state = self._state_lambda_body
            self._state(token)

    def _state_lambda_param_list(self, token):
        """Handle lambda parameter list with proper bracket tracking."""
        if token == '(':
            self.bracket_stack.append('(')
        elif token == ')':
            if self.bracket_stack and self.bracket_stack[-1] == '(':
                self.bracket_stack.pop()
                if not self.bracket_stack:
                    # End of parameter list, check for body
                    self._state = self._state_lambda_body
        elif token in ('<', '['):
            self.bracket_stack.append(token)
        elif token == '>' and self.bracket_stack and self.bracket_stack[-1] == '<':
            self.bracket_stack.pop()
        elif token == ']' and self.bracket_stack and self.bracket_stack[-1] == '[':
            self.bracket_stack.pop()

    def _state_lambda_body(self, token):
        """Skip qualifiers and return types until the body or declaration ends."""
        if token == '{':
            # Start of lambda body
            self.bracket_stack.append('{')
            self._state = self._state_lambda_body_skip
        elif token in (';', ',', ')'):
            # Lambda declaration ended, return to global
            self._state = self._state_global
            self._state(token)

    def _state_lambda_body_skip(self, token):
        """Skip lambda body with proper brace tracking."""
        if token == '{':
            self.bracket_stack.append('{')
        elif token == '}':
            if self.bracket_stack and self.bracket_stack[-1] == '{':
                self.bracket_stack.pop()
                if not self.bracket_stack:
                    # End of lambda body
                    self._state = self._state_global

    def _state_lambda_capture(self, token):
        """Handle lambda capture list."""
        if token == ']':
            # End of capture list, now expect parameters
            self._state = self._state_lambda_params
        # Otherwise, continue in capture list
