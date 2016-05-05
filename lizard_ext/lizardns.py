"""
This extension counts nested control structures within a function.
The extension is implemented with C++ in mind.

The code borrows heavily from implementation of Nesting Depth extension
originally written by Mehrdad Meh and Terry Yin.
"""

from lizard import FileInfoBuilder, FunctionInfo
from lizardnd import patch, patch_append_method


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

        structure_indicator = "{"
        structure_end = "}"
        indent_indicator = ";"

        for token in tokens:
            if token in structures:
                reader.context.add_nested_structure(token)

            elif token == structure_indicator:
                reader.context.add_brace()

            elif token == structure_end:
                reader.context.pop_brace()
                reader.context.pop_nested_structure();

            elif token == indent_indicator:
                reader.context.pop_nested_structure();

            yield token


class NSFileInfoAddition(FileInfoBuilder):

    def add_nested_structure(self, token):
        """Conditionally adds nested structures."""
        # Handle compound else-if.
        if token == "if" and self.current_function.structure_stack:
            prev_token, br_state = self.current_function.structure_stack[-1]
            if (prev_token == "else" and
                    br_state == self.current_function.brace_count):
                return

        self.current_function.structure_stack.append(
            (token, self.current_function.brace_count))

        ns_cur = len(self.current_function.structure_stack)
        if self.current_function.max_nested_structures < ns_cur:
            self.current_function.max_nested_structures = ns_cur


    def pop_nested_structure(self):
        """Conditionally pops the structure count if braces match."""
        if not self.current_function.structure_stack:
            return

        _, br_state = self.current_function.structure_stack[-1]
        if br_state == self.current_function.brace_count:
            self.current_function.structure_stack.pop()

    def add_brace(self):
        self.current_function.brace_count += 1

    def pop_brace(self):
        # TODO: For some reason, brace count goes negative.
        self.current_function.brace_count -= 1


def _init_nested_structure_data(self, *_):
    self.max_nested_structures = 0
    self.brace_count = 0
    self.structure_stack = []


patch(NSFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_nested_structure_data, FunctionInfo, "__init__")
