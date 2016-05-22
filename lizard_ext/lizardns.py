"""
This extension counts nested control structures within a function.

The extension is implemented with C++ and Python in mind,
but it is expected to work with other languages supported by Lizard
with its language reader implementing 'nesting_level' metric for tokens.

The code borrows heavily from implementation of Nesting Depth extension
originally written by Mehrdad Meh and Terry Yin.
"""

from lizard import FunctionInfo
from lizard_ext.lizardnd import patch_append_method


DEFAULT_NS_THRESHOLD = 3


class LizardExtension(object):  # pylint: disable=R0903

    FUNCTION_INFO = {"max_nested_structures": {"caption": "  NS  "}}

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

        The implementation relies on nesting level metric for tokens
        provided by language readers.
        If the following contract for the nesting level metric does not hold,
        this implementation of nested structure counting is invalid.

        If a control structure has started its block (eg. '{'),
        and its level is **less** than the next structure,
        the next structure is nested.

        If a control structure has *not* started its block,
        and its level is **no more** than the next structure,
        the next structure is nested (compound statement).

        If a control structure level is **higher** than the next structure,
        it is considered closed.

        If a control structure has started its block,
        and its level is **equal** to the next structure,
        it is considered closed.

        The level of any non-structure tokens is treated
        with the same logic as for the next structures
        for control block **starting** and **closing** purposes.
        """
        # TODO: Delegate this to language readers  # pylint: disable=fixme
        structures = set(['if', 'else', 'elif', 'for', 'foreach', 'while',
                          'do', 'try', 'catch', 'switch', 'finally',
                          'except', 'with'])

        cur_level = 0
        start_structure = [False]  # Just to make it mutable.
        structure_stack = []  # [(token, ns_level)]

        def add_nested_structure(token):
            """Conditionally adds nested structures."""
            if structure_stack:
                prev_token, ns_level = structure_stack[-1]
                if cur_level == ns_level:
                    if (token == "if" and prev_token == "else" and
                            not start_structure[0]):
                        return  # Compound 'else if' in C-like languages.
                    if start_structure[0]:
                        structure_stack.pop()
                elif cur_level < ns_level:
                    while structure_stack and ns_level >= cur_level:
                        _, ns_level = structure_stack.pop()

            structure_stack.append((token, cur_level))
            start_structure[0] = False  # Starts on the next level with body.

            ns_cur = len(structure_stack)
            if reader.context.current_function.max_nested_structures < ns_cur:
                reader.context.current_function.max_nested_structures = ns_cur

        def pop_nested_structure():
            """Conditionally pops the nested structures if levels match."""
            if not structure_stack:
                return

            _, ns_level = structure_stack[-1]

            if cur_level > ns_level:
                start_structure[0] = True

            elif cur_level < ns_level:
                while structure_stack and ns_level >= cur_level:
                    _, ns_level = structure_stack.pop()
                start_structure[0] = bool(structure_stack)

            elif start_structure[0]:
                structure_stack.pop()

        for token in tokens:
            cur_level = reader.context.current_nesting_level
            if token in structures:
                add_nested_structure(token)
            else:
                pop_nested_structure()

            yield token


def _init_nested_structure_data(self, *_):
    self.max_nested_structures = 0


patch_append_method(_init_nested_structure_data, FunctionInfo, "__init__")
