"""Token roles for lambdas in a C++ or Java control header."""


class ConditionLambdaBrace:  # pylint: disable=R0903
    """Find lambda body braces while a C++ or Java control header is open."""

    _OPERAND_STARTS = frozenset((
        '(', '[', '{', ',', ';', '?', ':', '=', 'return', 'co_return',
        '&&', '||', '!', '~', '==', '!=', '<', '<=', '>', '>=', '<=>',
        '+', '-', '*', '/', '%', '&', '|', '^', '<<', '>>',
    ))
    _GROUPS = {'(': ')', '[': ']', '{': '}'}

    def __init__(self, cpp):
        self.cpp = cpp
        self.opens_body = False
        self.previous = None
        self.head = False
        self.capture = False
        self.closers = []
        self.requires_body = False

    def track(self, tokens, context):
        """Observe each token before nesting reads it."""
        for token in tokens:
            self.observe(token, context)
            yield token

    def observe(self, token, context):
        """Detect a lambda brace in the current control header."""
        self.opens_body = (context.current_function.control_header_depth is not None
                           and self.read(token))

    def read(self, token):
        previous, self.previous = self.previous, token
        if not self.cpp:
            return previous == '->' and token == '{'
        if self.closers:
            self._read_group(token, previous)
        elif self.head:
            return self._read_declarator(token, previous)
        elif token == '[' and previous in self._OPERAND_STARTS:
            self.head = self.capture = True
            self.closers.append(']')
            self.requires_body = False
        return False

    def _read_group(self, token, previous):
        if self.capture and token == '[' and previous == '[' and self.closers == [']']:
            self.head = False
        if token in self._GROUPS:
            self.closers.append(self._GROUPS[token])
        elif token == '<' and self.closers[-1] == '>':
            self.closers.append('>')
        elif token == '>>' and self.closers[-2:] == ['>', '>']:
            del self.closers[-2:]
        elif token == self.closers[-1]:
            self.closers.pop()
        if not self.closers:
            self.capture = False

    def _read_declarator(self, token, previous):
        if token == '{':
            if self.requires_body:
                # A requires-expression in the constraint owns this group.
                self.requires_body = False
                self.closers.append('}')
            else:
                self.head = False
                return True
        elif token in ('(', '['):
            self.closers.append(self._GROUPS[token])
        elif token == '<':
            self.closers.append('>')
        elif token == 'requires':
            # Here a constraint can start with a requires-expression. A
            # requires after a complete operand starts a new clause instead.
            self.requires_body = previous in ('requires', '&&', '||', 'and', 'or')
        elif token in (')', '}', ',', ';'):
            self.head = False
        return False
