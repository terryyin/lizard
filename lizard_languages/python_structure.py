"""Python indentation and function declaration states."""

from .code_reader import CodeStateMachine
from .python_tokens import FStringExpressionToken


def count_spaces(token):
    return len(token.replace('\t', ' ' * 8))


class PythonIndents:  # pylint: disable=R0902
    def __init__(self, context):
        self.indents = [0]
        self.context = context

    def set_nesting(self, spaces, token=""):
        while self.indents[-1] > spaces and (not token.startswith(")")):
            self.indents.pop()
            self.context.pop_nesting()
        if self.indents[-1] < spaces:
            self.indents.append(spaces)
            self.context.add_bare_nesting()

    def reset(self):
        self.set_nesting(0)


class PythonStates(CodeStateMachine):  # pylint: disable=R0903
    read_return_annotations = True

    supports_inline_suites = True
    ignore_indentation = False
    inline_suite = False

    def __init__(self, context, reader):
        super(PythonStates, self).__init__(context)
        self.reader = reader
        self.lambda_heads = 0

    def _state_global(self, token):
        if token == 'def':
            self._state = self._function

    def _function(self, token):
        if token == '[':
            # PEP 695 `def f[T](...)`. Do not restart on each token, or ']'
            # becomes the name. Defer '(' so [T] appears in long_name.
            self.br_count = 0
            self.next(self._state_type_parameters, token)
        elif token != '(':
            self.context.restart_new_function(token)
        else:
            self.context.add_to_long_function_name("(")
            self._state = self._dec

    def _state_type_parameters(self, token):
        # Consume a type param list, tracking nesting so bounds like
        # `def f[T: list[int]](x)` work.
        if token in ('def', 'class'):
            # Unbalanced '[': truncated or invalid source. Neither keyword can
            # appear inside a type param list, so treat this as the next
            # definition instead of swallowing the rest of the file.
            self.br_count = 0
            self.next(self._state_global, token)
            return
        self.context.add_to_long_function_name(" " + token)
        self.br_count += {'[': 1, ']': -1}.get(token, 0)
        if self.br_count == 0:
            self._state = self._function

    def _dec(self, token):
        if isinstance(token, FStringExpressionToken):
            self._token_in_a_parameter(token)
            return
        if token == ')':
            self._state = self._state_colon
        elif token == '[':
            self.br_count = 1
            self._state = self._state_parameterized_type_annotation
        elif token in ('(', '{', 'lambda'):
            # A default value in brackets, or a lambda: its commas do not
            # separate the parameters of the function.
            self.br_count = 0
            self.lambda_heads = 0
            self.next(self._state_default_value, token)
            return
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _token_in_a_parameter(self, token):
        function = self.context.current_function
        function.add_to_long_name(" " + token)
        function.full_parameters[-1] += " " + token

    def _state_default_value(self, token):
        if isinstance(token, FStringExpressionToken):
            # Its brackets and colons cannot close the surrounding default
            # expression or lambda argument head.
            self._token_in_a_parameter(token)
            return
        if token == ',':
            self._token_in_a_parameter(token)
        else:
            self.context.parameter(token)
        if token in ('(', '[', '{'):
            self.br_count += 1
        elif token in (')', ']', '}'):
            self.br_count -= 1
        elif token == 'lambda' and self.br_count == 0:
            self.lambda_heads += 1
        elif token == ':' and self.br_count == 0 and self.lambda_heads:
            self.lambda_heads -= 1
        if self.br_count < 0 or (
                self.br_count == 0 and self.lambda_heads == 0):
            self._state = self._dec

    def _state_colon(self, token):
        if token == ':':
            self.next(self._state_first_line)
        elif self.read_return_annotations and token == '->':
            self.ignore_indentation = True
            self.br_count = 0
            self.lambda_heads = 0
            self.next(self._state_return_annotation)
        elif self.read_return_annotations and token == '\\\n':
            return
        else:
            self.next(self._state_global)

    def _state_return_annotation(self, token):
        if isinstance(token, FStringExpressionToken):
            return
        if token in ('def', 'class'):
            self.ignore_indentation = False
            self.next(self._state_global, token)
        elif token in ('(', '[', '{'):
            self.br_count += 1
        elif token in (')', ']', '}'):
            self.br_count -= 1
        elif self.br_count == 0:
            if token == 'lambda':
                self.lambda_heads += 1
            elif token == ':':
                if self.lambda_heads:
                    self.lambda_heads -= 1
                else:
                    self.ignore_indentation = False
                    self.next(self._state_first_line)

    def _state_first_line(self, token):
        self.inline_suite = (
            self.supports_inline_suites and token != '\n' and
            self.context.pending_function is self.context.current_function)
        if token.startswith('"""') or token.startswith("'" * 3):
            newlines = token.count('\n')
            self.context.add_nloc(-newlines - (not self.inline_suite))
            if self.inline_suite and newlines:
                self.next(self._state_global)
                self.sub_state(self._state_after_inline_docstring)
                return
        self.next(self._state_global, token)

    def _state_after_inline_docstring(self, token):
        if token == ';':
            return
        if token not in ('\n', '\\\n'):
            # The docstring excluded this line, which also contains code.
            self.context.add_nloc(1)
        self.next(self.saved_state, token)

    def end_inline_line(self):
        # Inline suites have no body indentation to attach the function.
        self.context.add_bare_nesting()
        self.context.pop_nesting()
        self.inline_suite = False
        self.next(self._state_global)

    def _state_parameterized_type_annotation(self, token):
        # Up to the "]" that closes the first "[": Callable[[int, str], int]
        self.context.add_to_long_function_name(" " + token)
        if isinstance(token, FStringExpressionToken):
            return
        self.br_count += {'[': 1, ']': -1}.get(token, 0)
        if self.br_count == 0:
            self._state = self._dec
