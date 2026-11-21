"""
This is an extension of lizard, that counts the 'Nesting Depth'
in every function.
"""
from lizard import FileInfoBuilder, FunctionInfo

DEFAULT_ND_THRESHOLD = 7


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

    def __call__(self, tokens, reader, l_depth=0):  # pylint: disable=R0912
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
        implied_semicolons = getattr(reader, "implied_semicolons", None)
        if implied_semicolons:
            implied_semicolon_before = implied_semicolons(reader.context).before
        for token in tokens:
            if implied_semicolons and implied_semicolon_before(token):
                # The statement ends here as it does at a ";".
                reader.context.clear_prev_was_else()
                if reader.context.get_condition_depth() == 0:
                    l_depth = reader.context.end_statement()
            if token in block_ends:
                l_depth = reader.context.close_block()
            if token in block_starts or token == indent_indicator:
                reader.context.clear_prev_was_else()
            # Don't add nesting for "if" when it immediately follows "else" (else if)
            # Syntactically else if is equivalent to else {if} but should not increase depth
            if token == 'else' and not getattr(token, 'is_member_name', False):
                reader.context.set_prev_was_else(True)
            elif token == 'if' and token in loops and reader.context.get_prev_was_else():
                reader.context.clear_prev_was_else()
            # Handle opening parenthesis - start of condition
            elif token == '(':
                reader.context.set_in_condition(True)
                reader.context.increment_condition_depth()
                reader.context.set_logical_operator_added(False)  # Reset for new condition
            # Handle closing parenthesis - end of condition
            elif token == ')':
                reader.context.decrement_condition_depth()
                if reader.context.get_condition_depth() == 0:
                    reader.context.set_in_condition(False)
                    reader.context.set_logical_operator_added(False)

            # Handle logical operators && and || within conditions
            elif token in ('&&', '||'):
                if reader.context.get_in_condition():
                    # Only add nesting depth for the first logical operator in a condition
                    # Subsequent && or || operators in the same condition don't add depth
                    if not reader.context.get_logical_operator_added():
                        l_depth = reader.context.open_level()
                        reader.context.set_logical_operator_added(True)
                else:
                    # Not in a condition, treat as regular nesting
                    l_depth = reader.context.open_level()

            # Handle other loop keywords (if, for, while, etc.)
            elif token in loops and token not in ('&&', '||') \
                    and not getattr(token, 'is_optional', False) \
                    and not getattr(token, 'is_member_name', False):
                l_depth = reader.context.open_level()

            if token in block_starts:
                reader.context.open_block()
            if token == indent_indicator and \
                    reader.context.get_condition_depth() == 0:
                l_depth = reader.context.end_statement()
            if l_depth < 0:
                l_depth = 0
                reader.context.reset_nd_complexity()
            yield token


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
