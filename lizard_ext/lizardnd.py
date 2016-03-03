"""
This is an extension of lizard, that counts the 'Nesting Depth'
in every function.

    parser.add_argument("-N", "--ND",
                        help='''Threshold for nesting depth number
                        warning. The default value is %d.
                        Functions with ND bigger than it will generate warning
                        ''' % DEFAULT_ND_THRESHOLD,
                        type=int,
                        dest="ND",
                        default=DEFAULT_ND_THRESHOLD)

"""


class LizardExtension(object):  # pylint: disable=R0903

    def __call__(self, tokens, reader, l_depth=0):  # pylint: disable=R0912
        if hasattr(reader, "loops"):
            loops = reader.loops
        else:
            loops = set(['if', 'else', 'foreach', 'for', 'while', '&&', '||',
                         '?', 'catch', 'case', 'try'])
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
    if l_depth == 1:
        reader.context.add_nd_condition(-1)
