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
            if token in loops:
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

    def add_hidden_bracket_condition(self, inc=1):
        self.current_function.hidden_bracket += inc

    def get_hidden_bracket(self):
        return self.current_function.hidden_bracket

    def loop_bracket_status(self):
        tmp_bracket_loop = self.current_function.bracket_loop
        self.current_function.bracket_loop = not tmp_bracket_loop

    def get_loop_status(self):
        return self.current_function.bracket_loop


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


patch(NDFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_nesting_depth_data, FunctionInfo, "__init__")
