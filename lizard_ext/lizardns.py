"""
This extension counts nested control structures within a function.
The extension is implemented with C++ in mind.

The code borrows heavily from implementation of Nesting Depth extension
originally written by Mehrdad Meh and Terry Yin.
"""

from lizard import FunctionInfo
from lizard_ext.lizardnd import patch_append_method


DEFAULT_NS_THRESHOLD = 3


class LizardExtension(object):  # pylint: disable=R0903

    FUNCTION_CAPTION = ["  NS  "]
    FUNCTION_INFO_PART = ["max_nested_structures"]

    @staticmethod
    def set_args(parser):
        parser.add_argument(
            "--NS",
            help='''Threshold for the number of nested control structures.
            The default value is %d.
            ''' % DEFAULT_NS_THRESHOLD,
            type=int,
            dest="NS",
            default=DEFAULT_NS_THRESHOLD)

    def __call__(self, tokens, reader):
        """The intent of the code is to detect control structures as entities.

        The complexity arises from tracking of
        control structures without brackets.
        The termination of such control structures in C-like languages
        is the next statement or control structure with a compound statement.

        Moreover, control structures with two or more tokens complicates
        the proper counting, for example, 'else if'.

        In Python with meaningful indentation,
        tracking the indentation levels becomes crucial
        to identify boundaries of the structures.
        The following code is not designed for Python.
        """
        structures = set(['if', 'else', 'foreach', 'for', 'while', 'do',
                          'try', 'catch', 'switch'])

        brace_count = 0
        paren_count = 0
        structure_stack = []

        def add_nested_structure(token):
            """Conditionally adds nested structures."""
            # Handle compound else-if.
            if token == "if" and structure_stack:
                prev_token, br_state = structure_stack[-1]
                if prev_token == "else" and br_state == brace_count:
                    return

            structure_stack.append((token, brace_count))

            ns_cur = len(structure_stack)
            if reader.context.current_function.max_nested_structures < ns_cur:
                reader.context.current_function.max_nested_structures = ns_cur

        def pop_nested_structure():
            """Conditionally pops the structure count if braces match."""
            if not structure_stack:
                return

            _, br_state = structure_stack[-1]
            if br_state == brace_count:
                structure_stack.pop()

        structure_indicator = "{"
        structure_end = "}"
        indent_indicator = ";"

        for token in tokens:
            if structure_stack:  # Inside of the control structure.
                if token == "(":
                    paren_count += 1
                elif token == ")":
                    assert paren_count > 0
                    paren_count -= 1

            if paren_count == 0:  # Ignore if inside parentheses.
                if token in structures:
                    add_nested_structure(token)

                elif token == structure_indicator:
                    brace_count += 1

                elif token == structure_end:
                    # TODO: assert brace_count > 0  # pylint: disable=fixme
                    brace_count -= 1
                    pop_nested_structure()

                elif token == indent_indicator:
                    pop_nested_structure()

            yield token


def _init_nested_structure_data(self, *_):
    self.max_nested_structures = 0


patch_append_method(_init_nested_structure_data, FunctionInfo, "__init__")
