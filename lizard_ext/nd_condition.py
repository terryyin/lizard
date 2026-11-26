"""Token roles for lambdas in a C++ or Java control header."""


def java_lambda_expressions(tokens, context, lambda_brace):
    """An expression lambda owns the operators up to its argument boundary."""
    expressions = []
    after_arrow = False
    type_arguments = _JavaTypeArguments()
    for token in tokens:
        if after_arrow and token != '{':
            expressions.append(_JavaLambdaExpression(context.current_function))
        if expressions and not type_arguments.read(token) and token in (')', ',', '?', ':'):
            while expressions and expressions[-1].ends_at(token, context.current_function):
                expressions.pop().close(context)
        after_arrow = token == '->' and context.current_function.in_control_header
        lambda_brace.observe(token, context)
        yield token


class _JavaTypeArguments:  # pylint: disable=R0903
    """Recognize invocation and construction type arguments in an expression."""

    def __init__(self):
        self.previous = None
        self.constructed_type = False
        self.angles = 0
        self.annotation_groups = 0

    def read(self, token):
        previous, self.previous = self.previous, token
        if self.angles:
            if token == '(':
                self.annotation_groups += 1
            elif token == ')':
                self.annotation_groups -= 1
            elif not self.annotation_groups and token[0] in '<>':
                self.angles += token.count('<') - token.count('>')
            return True
        if token.startswith('<') and (self.constructed_type or previous in ('.', '::')):
            self.angles = token.count('<') - token.count('>')
            return True
        self.constructed_type = (token == 'new' or
                                 self.constructed_type and (token == '.' or token.isidentifier()))
        return False


class _JavaLambdaExpression:  # pylint: disable=R0903
    def __init__(self, function):
        self.function = function
        self.condition_depth = function.condition_depth
        self.block_depth = len(function.block_levels)
        self.levels = function.levels_awaiting_body
        self.logical_added = function.logical_operator_added
        self.ternaries = 0
        function.logical_operator_added = False

    def ends_at(self, token, function):
        if (function is not self.function or function.condition_depth != self.condition_depth
                or len(function.block_levels) != self.block_depth):
            return False
        if token == '?':
            self.ternaries += 1
            return False
        if token == ':' and self.ternaries:
            self.ternaries -= 1
            return False
        return True

    def close(self, context):
        function = self.function
        levels = max(function.levels_awaiting_body - self.levels, 0)
        function.levels_awaiting_body -= levels
        function.logical_operator_added = self.logical_added
        context.add_nd_condition(-levels)


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
