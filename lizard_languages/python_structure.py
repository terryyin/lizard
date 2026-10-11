"""Python indentation and function declaration states."""

from .code_reader import CodeStateMachine


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
    def __init__(self, context, reader):
        super(PythonStates, self).__init__(context)
        self.reader = reader

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
        if token == ')':
            self._state = self._state_colon
        elif token == '[':
            self._state = self._state_parameterized_type_annotation
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _state_colon(self, token):
        if token == ':':
            self.next(self._state_first_line)
        else:
            self.next(self._state_global)

    def _state_first_line(self, token):
        self._state = self._state_global
        if token.startswith('"""') or token.startswith("'''"):
            self.context.add_nloc(-token.count('\n') - 1)
        self._state_global(token)

    def _state_parameterized_type_annotation(self, token):
        self.context.add_to_long_function_name(" " + token)
        if token == ']':
            self._state = self._dec
