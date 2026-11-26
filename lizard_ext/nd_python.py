"""Nesting depth for Python suites and expressions."""
from lizard_languages.python_tokens import FStringExpressionToken


_PYTHON_SUITES = frozenset(('if', 'elif', 'else', 'for', 'while', 'try',
                            'except', 'finally', 'with'))


def python_depth(tokens, reader):
    depth = _PythonDepth(reader)
    for token in tokens:
        depth.read(token)
        yield token


class _PythonDepth:  # pylint: disable=R0903
    """Track Python suites and the expressions that run inside them."""

    def __init__(self, reader):
        self.reader = reader
        self.context = reader.context
        self.last_line = None
        self.statement_start = True
        self.in_header = False
        self.declaring_function = None
        self.expressions = [_PythonExpression()]

    def read(self, token):
        line = self.context.current_line
        interpolation = isinstance(token, FStringExpressionToken)
        if (line != self.last_line and len(self.expressions) == 1 and not interpolation and
                line - token.count('\n') != self.last_line):
            self._start_statement()
        self.last_line = line
        if token == '\\\n' or (self.statement_start and token == 'async'):
            return
        function = self.declaring_function or self.context.current_function
        if self.statement_start and token == 'def':
            # The declaration and its defaults execute in the enclosing scope.
            function.max_nesting_depth = max(function.max_nesting_depth, function.nesting_depth + 1)
            self.declaring_function = function
        elif self.statement_start and (token in _PYTHON_SUITES or
                                       (token == 'case' and getattr(self.reader, '_keyword_case', False))):
            depth = _increase_python_depth(function)
            function.block_levels.append((self.context.current_nesting_level, depth))
            self.in_header = True
        elif token in ('if', 'for'):
            self.expressions[-1].add_decision(function, token)
        self.statement_start = False
        if not interpolation:
            self._read_delimiter(function, token)

    def _start_statement(self):
        self.declaring_function = None
        function = self.context.current_function
        suites = function.block_levels
        indent = self.context.current_nesting_level
        while suites and suites[-1][0] >= indent:
            suites.pop()
        self._end_expression(function)
        self.statement_start = True

    def _read_delimiter(self, function, token):
        expression = self.expressions[-1]
        if token in ('(', '[', '{'):
            self.expressions.append(_PythonExpression())
        elif token in (')', ']', '}') and len(self.expressions) > 1:
            self.expressions.pop().finish(function)
            self.expressions[-1].include_group(expression.peak)
        elif token == ',' and not expression.comprehension and not expression.lambda_heads:
            expression.finish(function)
        elif token == 'else':
            expression.prefix = 0
        elif token == 'lambda':
            expression.lambda_heads += 1
        elif token == ':' and expression.lambda_heads:
            expression.lambda_heads -= 1
        elif len(self.expressions) == 1 and (
                token == ';' or token == ':' and (self.in_header or self.declaring_function is not None)):
            self._end_expression(function)
            self.declaring_function = None

    def _end_expression(self, function):
        suites = function.block_levels
        function.nesting_depth = suites[-1][1] if suites else 0
        self.expressions = [_PythonExpression()]
        self.in_header = False


class _PythonExpression:
    """Separate peer operands and keep the peak of a nested group."""

    def __init__(self):
        self.levels = self.peak = self.prefix = self.lambda_heads = 0
        self.comprehension = False

    def add_decision(self, function, token):
        self.levels += 1
        self.peak = max(self.peak, self.levels + self.prefix)
        _increase_python_depth(function, self.prefix)
        self.prefix = 0
        self.comprehension |= token == 'for'

    def include_group(self, peak):
        self.peak = max(self.peak, self.levels + peak)
        self.prefix = max(self.prefix, peak)

    def finish(self, function):
        function.nesting_depth -= self.levels
        self.levels = self.prefix = 0


def _increase_python_depth(function, prefix=0):
    function.nesting_depth += 1
    function.max_nesting_depth = max(function.max_nesting_depth, function.nesting_depth + prefix)
    return function.nesting_depth
