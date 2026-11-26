"""
This is an extension of lizard, that counts the 'Nesting Depth'
in every function.
"""
from lizard import FileInfoBuilder, FunctionInfo
from lizard_languages.javascript_statements import ImpliedSemicolons

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

    def __call__(self, tokens, reader, l_depth=0):
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
        if 'cpp' in getattr(reader, 'language_names', ()):
            tokens = _cpp_grouped_commas(tokens, reader.context)
        block_starts = getattr(reader, "block_starts", ('{',))
        indent_indicator = getattr(reader, 'indent_indicator', ';')
        standalone_chains = (isinstance(implied_semicolons, ImpliedSemicolons)
                             or not {'cpp', 'java'}.isdisjoint(getattr(reader, 'language_names', ())))
        # Most tokens cannot change the depth: one lookup skips them.
        depth_tokens = frozenset(
            (indent_indicator, 'else', 'if', '(', ')', '&&', '||')
            + (tuple(_STANDALONE_BOUNDARIES) if standalone_chains else ())
        ).union(block_ends, block_starts, loops)
        context = reader.context
        previous_token = None
        for token in tokens:
            if implied_semicolons and implied_semicolons.before_token:
                # The statement ends here as it does at a ";".
                context.clear_prev_was_else()
                if context.get_condition_depth() == 0:
                    l_depth = context.end_statement()
            if token in depth_tokens:
                if (standalone_chains and token in _STANDALONE_BOUNDARIES
                        and not isinstance(token, _DeferredCppComma)):
                    l_depth = context.read_standalone_boundary(token, l_depth)
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
                    context.open_condition(
                        standalone_chains and (
                            previous_token in _GROUP_OPERAND_STARTS or
                            (implied_semicolons and implied_semicolons.before_token and
                             context.get_condition_depth() == 0)))
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
                    elif not standalone_chains or not context.current_function.standalone_logical_added:
                        # Not in a condition, treat as regular nesting
                        l_depth = context.open_level()
                        context.current_function.standalone_logical_added = standalone_chains

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
            previous_token = token
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
        self.current_function.standalone_logical_added = False
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
        function.standalone_logical_added = False

    def close_block(self):
        """A block end releases the levels its block took over."""
        function = self.current_function
        levels = function.block_levels
        function.standalone_logical_added = False
        return self.add_nd_condition(-(levels.pop() if levels else 1))

    def end_statement(self):
        """A statement ends the levels of a braceless body."""
        levels = self.current_function.levels_awaiting_body
        self.current_function.levels_awaiting_body = 0
        self.current_function.standalone_logical_added = False
        return self.add_nd_condition(-levels)

    def set_in_condition(self, in_condition):
        self.current_function.in_condition = in_condition

    def get_in_condition(self):
        return self.current_function.in_condition

    def open_condition(self, can_group_standalone=False):
        """A "(" opens a condition, or a level inside one."""
        function = self.current_function
        function.condition_depth += 1
        if (can_group_standalone and
                function.condition_depth == len(function.standalone_groups) + 1):
            # Grouping an operand keeps the surrounding statement's chain.
            function.standalone_groups.append(_StandaloneGroup(function))
            return
        function.in_condition = True
        function.logical_operator_added = False

    def close_condition(self):
        """A ")" closes a level, and the condition with its last level."""
        self.decrement_condition_depth()
        _close_condition_at_depth(self.current_function)

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
                _close_condition_at_depth(enclosing)

    def get_condition_depth(self):
        return self.current_function.condition_depth

    def read_standalone_boundary(self, token, depth):
        function = self.current_function
        if token == '[':
            function.standalone_square_depth += 1
        elif token == ']':
            function.standalone_square_depth = max(function.standalone_square_depth - 1, 0)
        elif function.condition_depth == 0:
            function.standalone_logical_added = False
        elif token == ',':
            group = _standalone_group(function)
            if group is not None:
                levels = max(function.levels_awaiting_body - group.levels, 0)
                function.levels_awaiting_body -= levels
                function.standalone_logical_added = group.logical_added
                return self.add_nd_condition(-levels)
        return depth

    def reset_condition_tracking(self):
        self.current_function.in_condition = False
        self.current_function.condition_depth = 0
        self.current_function.logical_operator_added = False
        self.current_function.standalone_groups = []
        self.current_function.standalone_square_depth = 0

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


_STATEMENT_EXPRESSION_SEPARATORS = frozenset(('=', ',', ':', '?'))
_STANDALONE_BOUNDARIES = _STATEMENT_EXPRESSION_SEPARATORS | {'[', ']'}
_GROUP_OPERAND_STARTS = _STATEMENT_EXPRESSION_SEPARATORS | {
    None, 'return', 'co_return', 'throw', 'yield',
    '(', '[', '{', ';', '&&', '||', '??', '!', '~', '+', '-', '*', '/', '%',
    '|', '&', '^', '==', '!=', '===', '!==',
}


def _close_condition_at_depth(function):
    if function.condition_depth <= len(function.standalone_groups):
        del function.standalone_groups[function.condition_depth:]
        function.in_condition = False
        function.logical_operator_added = False


class _StandaloneGroup:  # pylint: disable=R0903
    def __init__(self, function):
        self.levels = function.levels_awaiting_body
        self.logical_added = function.standalone_logical_added
        self.square_depth = function.standalone_square_depth
        self.block_depth = len(function.block_levels)

    def at_surface_of(self, function):
        return (function.standalone_square_depth == self.square_depth
                and len(function.block_levels) == self.block_depth)


def _standalone_group(function):
    groups = function.standalone_groups
    if groups and function.condition_depth == len(groups) and groups[-1].at_surface_of(function):
        return groups[-1]
    return None


def _cpp_grouped_commas(tokens, context):
    """Defer a possible type separator until the surrounding syntax is clear."""
    group = None
    previous = before_previous = ''
    for token in tokens:
        if group is not None and not group.read(token):
            if group.has_comma and not group.is_call(token):
                context.read_standalone_boundary(',', context.current_function.nesting_depth)
            group = None
        if (group is None and token == '<' and previous.isidentifier()
                and _standalone_group(context.current_function) is not None):
            group = _CppTemplateArguments(before_previous == 'template')
        before_previous, previous = previous, token
        yield _DeferredCppComma(token) if group is not None and token == ',' else token


class _DeferredCppComma(str):
    """A comma whose operand boundary is decided by the C++ token stream."""


class _CppTemplateArguments:
    _TYPE_WORDS = frozenset((
        'bool', 'char', 'char8_t', 'char16_t', 'char32_t', 'double', 'float',
        'int', 'long', 'short', 'signed', 'unsigned', 'void', 'wchar_t',
        'const', 'volatile', 'typename', 'class', 'struct', 'union', 'enum',
    ))
    _TYPE_PUNCTUATION = frozenset(('::', '*', '&', '...'))
    _EXPRESSION_KEYWORDS = frozenset((
        'if', 'else', 'foreach', 'for', 'while', 'catch', 'case', 'try', 'def', 'new',
    ))

    def __init__(self, explicit):
        self.depth = 1
        self.has_comma = False
        self.has_type = explicit

    def read(self, token):
        if not self.depth:
            return False
        if token == '<':
            self.depth += 1
        elif token == '>':
            self.depth -= 1
        elif token == ',':
            self.has_comma = True
        elif token in self._TYPE_WORDS:
            # A type in a nested template can still be part of a comparison operand.
            self.has_type |= self.depth == 1
        else:
            return (token in self._TYPE_PUNCTUATION or token.isdecimal() or
                    token.isidentifier() and token not in self._EXPRESSION_KEYWORDS)
        return True

    def is_call(self, token):
        return self.depth == 0 and self.has_type and token == '('


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
    self.standalone_logical_added = False
    self.standalone_groups = []
    self.standalone_square_depth = 0
    self.levels_awaiting_body = 0  # structure levels closed by their body
    self.block_levels = []  # structure levels each open block will close
    self.in_condition = False  # Track if we're inside a condition
    self.condition_depth = 0   # Track nesting depth within conditions
    self.logical_operator_added = False  # Track if we've added nesting for logical operators in current condition
    self.prev_was_else = False  # Track "else if" to not add nesting for the if (Issue #418)


patch(NDFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_nesting_depth_data, FunctionInfo, "__init__")
