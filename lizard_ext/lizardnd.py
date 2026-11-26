"""
This is an extension of lizard, that counts the 'Nesting Depth'
in every function.
"""
from lizard import FileInfoBuilder, FunctionInfo
from lizard_languages.python_tokens import FStringExpressionToken

DEFAULT_ND_THRESHOLD = 7
_PYTHON_SUITES = frozenset(('if', 'elif', 'else', 'for', 'while', 'try',
                            'except', 'finally', 'with'))


class LizardExtension(object):  # pylint: disable=R0903

    FUNCTION_INFO = {
            "max_nesting_depth": {
                "caption": "  ND  ",
                "average_caption": " Avg.ND "}}

    @staticmethod
    def set_args(parser):
        parser.add_argument(
            "-N", "--ND",
            help='''Threshold for nesting depth number
            warning. The default value is %d.
            Functions with ND bigger than it will generate warning
            ''' % DEFAULT_ND_THRESHOLD,
            type=int,
            dest="ND",
            default=DEFAULT_ND_THRESHOLD)

    def __call__(self, tokens, reader, l_depth=0):
        if 'python' in getattr(reader, 'language_names', ()):
            return _python_depth(tokens, reader)
        implied_semicolons = None
        if hasattr(reader, "implied_semicolons"):
            tokens, implied_semicolons = reader.implied_semicolons(tokens)
        return self._nesting_depth(tokens, reader, l_depth, implied_semicolons)

    @staticmethod
    def _nesting_depth(tokens, reader, l_depth, implied_semicolons):  # pylint: disable=R0912
        if hasattr(reader, "loops"):
            loops = reader.loops
        else:
            loops = set(['if', 'foreach', 'for', 'while', '&&', '||',
                         '?', 'catch', 'case', 'try', 'def'])
        block_ends = getattr(reader, "block_ends", ('}',))
        block_starts = getattr(reader, "block_starts", ('{',))
        if hasattr(reader, "indent_indicator"):
            indent_indicator = reader.indent_indicator
        else:
            indent_indicator = ';'
        # Most tokens cannot change the depth: one lookup skips them.
        depth_tokens = frozenset(
            (indent_indicator, 'else', 'if', '(', ')', '&&', '||')
        ).union(block_ends, block_starts, loops)
        context = reader.context
        for token in tokens:
            if implied_semicolons and implied_semicolons.before_token:
                # The statement ends here as it does at a ";".
                context.clear_prev_was_else()
                if context.get_condition_depth() == 0:
                    l_depth = context.end_statement()
            if token in depth_tokens:
                if token in block_ends:
                    l_depth = context.close_block()
                if token in block_starts or token == indent_indicator:
                    context.clear_prev_was_else()
                # Don't add nesting for "if" when it immediately follows "else" (else if)
                # Syntactically else if is equivalent to else {if} but should not increase depth
                if token == 'else' and not getattr(token, 'is_member_name', False):
                    context.set_prev_was_else(True)
                elif token == 'if' and token in loops and context.get_prev_was_else():
                    context.clear_prev_was_else()
                # Handle opening parenthesis - start of condition
                elif token == '(':
                    context.open_condition()
                # Handle closing parenthesis - end of condition
                elif token == ')':
                    context.close_condition()

                # Handle logical operators && and || within conditions
                elif token in ('&&', '||'):
                    if context.get_in_condition():
                        # Only add nesting depth for the first logical operator in a condition
                        # Subsequent && or || operators in the same condition don't add depth
                        if not context.get_logical_operator_added():
                            l_depth = context.open_level()
                            context.set_logical_operator_added(True)
                    else:
                        # Not in a condition, treat as regular nesting
                        l_depth = context.open_level()

                # Handle other loop keywords (if, for, while, etc.)
                elif token in loops and token not in ('&&', '||') \
                        and not getattr(token, 'is_optional', False) \
                        and not getattr(token, 'is_member_name', False):
                    l_depth = context.open_level()

                if token in block_starts:
                    context.open_block()
                if token == indent_indicator and \
                        context.get_condition_depth() == 0:
                    l_depth = context.end_statement()
            if l_depth < 0:
                l_depth = 0
                context.reset_nd_complexity()
            yield token


def _python_depth(tokens, reader):
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


class NDFileInfoAddition(FileInfoBuilder):

    def add_nd_condition(self, inc=1):
        self.current_function.nesting_depth += inc
        nd_tmp = self.current_function.nesting_depth
        if self.current_function.max_nesting_depth < nd_tmp:
            self.current_function.max_nesting_depth = nd_tmp
        return self.current_function.nesting_depth

    def reset_nd_complexity(self):
        self.current_function.nesting_depth = 0
        self.current_function.levels_awaiting_body = 0
        self.current_function.block_levels = []
        self.current_function.prev_was_else = False
        self.reset_condition_tracking()

    def open_level(self):
        """A structure opens a level that its body will close."""
        self.current_function.levels_awaiting_body += 1
        return self.add_nd_condition()

    def open_block(self):
        """The block takes over the levels awaiting their body."""
        function = self.current_function
        function.block_levels.append(function.levels_awaiting_body)
        function.levels_awaiting_body = 0

    def close_block(self):
        """A block end releases the levels its block took over."""
        levels = self.current_function.block_levels
        return self.add_nd_condition(-(levels.pop() if levels else 1))

    def end_statement(self):
        """A statement ends the levels of a braceless body."""
        levels = self.current_function.levels_awaiting_body
        self.current_function.levels_awaiting_body = 0
        return self.add_nd_condition(-levels)

    def set_in_condition(self, in_condition):
        self.current_function.in_condition = in_condition

    def get_in_condition(self):
        return self.current_function.in_condition

    def open_condition(self):
        """A "(" opens a condition, or a level inside one."""
        function = self.current_function
        function.in_condition = True
        function.condition_depth += 1
        function.logical_operator_added = False

    def close_condition(self):
        """A ")" closes a level, and the condition with its last level."""
        self.decrement_condition_depth()
        function = self.current_function
        if function.condition_depth == 0:
            function.in_condition = False
            function.logical_operator_added = False

    def increment_condition_depth(self):
        self.current_function.condition_depth += 1

    def decrement_condition_depth(self):
        function = self.current_function
        if function.condition_depth > 0:
            function.condition_depth -= 1
        elif self.stacked_functions:
            # The function did not read the "(" of this ")": the reader
            # started it inside a group that the function around it read.
            enclosing = self.stacked_functions[-1]
            if enclosing.condition_depth > 0:
                enclosing.condition_depth -= 1
                if enclosing.condition_depth == 0:
                    enclosing.in_condition = False
                    enclosing.logical_operator_added = False

    def get_condition_depth(self):
        return self.current_function.condition_depth

    def reset_condition_tracking(self):
        self.current_function.in_condition = False
        self.current_function.condition_depth = 0
        self.current_function.logical_operator_added = False

    def set_logical_operator_added(self, added):
        self.current_function.logical_operator_added = added

    def get_logical_operator_added(self):
        return self.current_function.logical_operator_added

    def set_prev_was_else(self, value):
        self.current_function.prev_was_else = value

    def get_prev_was_else(self):
        return getattr(self.current_function, 'prev_was_else', False)

    def clear_prev_was_else(self):
        self.current_function.prev_was_else = False


def get_method(cls, name):
    """ python3 doesn't need the __func__ to get the func of the
        method.
    """
    method = getattr(cls, name)
    if hasattr(method, "__func__"):
        method = method.__func__
    return method


def patch(frm, accept_class):
    for method in [k for k in frm.__dict__ if not k.startswith("_")]:
        setattr(accept_class, method, get_method(frm, method))


def patch_append_method(frm, accept_class, method_name):
    old_method = get_method(accept_class, method_name)

    def appended(*args, **kargs):
        old_method(*args, **kargs)
        frm(*args, **kargs)

    setattr(accept_class, method_name, appended)


def _init_nesting_depth_data(self, *_):
    self.nesting_depth = 0
    self.max_nesting_depth = 0
    self.levels_awaiting_body = 0  # structure levels closed by their body
    self.block_levels = []  # structure levels each open block will close
    self.in_condition = False  # Track if we're inside a condition
    self.condition_depth = 0   # Track nesting depth within conditions
    self.logical_operator_added = False  # Track if we've added nesting for logical operators in current condition
    self.prev_was_else = False  # Track "else if" to not add nesting for the if (Issue #418)


patch(NDFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_nesting_depth_data, FunctionInfo, "__init__")
