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
        if hasattr(reader, "bracket"):
            bracket = reader.bracket
        else:
            bracket = '}'
        if hasattr(reader, "loop_indicator"):
            loop_indicator = reader.loop_indicator
        else:
            loop_indicator = '{'
        if hasattr(reader, "indent_indicator"):
            indent_indicator = reader.indent_indicator
        else:
            indent_indicator = ';'
        for token in tokens:
            # Handle opening parenthesis - start of condition
            if token == '(':
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
                        l_depth = reader.context.add_nd_condition()
                        if not reader.context.get_loop_status():
                            reader.context.add_hidden_bracket_condition()
                            reader.context.loop_bracket_status()
                        reader.context.set_logical_operator_added(True)
                else:
                    # Not in a condition, treat as regular nesting
                    l_depth = reader.context.add_nd_condition()
                    if not reader.context.get_loop_status():
                        reader.context.add_hidden_bracket_condition()
                        reader.context.loop_bracket_status()
            
            # Handle other loop keywords (if, for, while, etc.)
            elif token in loops and token not in ('&&', '||'):
                l_depth = reader.context.add_nd_condition()
                if not reader.context.get_loop_status():
                    reader.context.add_hidden_bracket_condition()
                    reader.context.loop_bracket_status()
            
            if token == loop_indicator:
                reader.context.loop_bracket_status()
            if token == bracket:
                l_depth = reader.context.add_nd_condition(-1)
            if token == indent_indicator:
                hidden_brackets = reader.context.get_hidden_bracket()
                check_loop_brackets(reader, l_depth, hidden_brackets)
            if l_depth < 0:
                l_depth = 0
                reader.context.reset_nd_complexity()
            yield token


def check_loop_brackets(reader, l_depth, hidden_brackets):
    if hidden_brackets > 0:
        reader.context.add_hidden_bracket_condition(-1)
        l_depth = reader.context.add_nd_condition(-1)


class NDFileInfoAddition(FileInfoBuilder):

    def add_nd_condition(self, inc=1):
        self.current_function.nesting_depth += inc
        nd_tmp = self.current_function.nesting_depth
        if self.current_function.max_nesting_depth < nd_tmp:
            self.current_function.max_nesting_depth = nd_tmp
        return self.current_function.nesting_depth

    def reset_nd_complexity(self):
        self.current_function.nesting_depth = 0
        self.current_function.hidden_bracket = 0
        self.current_function.bracket_loop = False
        self.reset_condition_tracking()

    def add_hidden_bracket_condition(self, inc=1):
        self.current_function.hidden_bracket += inc

    def get_hidden_bracket(self):
        return self.current_function.hidden_bracket

    def loop_bracket_status(self):
        tmp_bracket_loop = self.current_function.bracket_loop
        self.current_function.bracket_loop = not tmp_bracket_loop

    def get_loop_status(self):
        return self.current_function.bracket_loop

    def set_in_condition(self, in_condition):
        self.current_function.in_condition = in_condition

    def get_in_condition(self):
        return self.current_function.in_condition

    def increment_condition_depth(self):
        self.current_function.condition_depth += 1

    def decrement_condition_depth(self):
        if self.current_function.condition_depth > 0:
            self.current_function.condition_depth -= 1

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
    self.hidden_bracket = 0
    self.bracket_loop = False
    self.in_condition = False  # Track if we're inside a condition
    self.condition_depth = 0   # Track nesting depth within conditions
    self.logical_operator_added = False  # Track if we've added nesting for logical operators in current condition


patch(NDFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_nesting_depth_data, FunctionInfo, "__init__")
