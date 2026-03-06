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
from lizard_languages.code_reader import CodeStateMachine
from .extension_base import ExtensionBase


DEFAULT_NS_THRESHOLD = 3


class LizardExtension(ExtensionBase):  # pylint: disable=R0903
    """The intent of the code is to detect control structures as entities.

    The implementation relies on nesting level metric for tokens
    provided by language readers.
    If the following contract for the nesting level metric does not hold,
    this implementation of nested structure counting is invalid.

    If a control structure has started its block (e.g., '{'),
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

    FUNCTION_INFO = {"max_nested_structures": {"caption": "  NS  "}}

    # TODO: Delegate this to language readers  # pylint: disable=fixme
    structures = set(['if', 'else', 'elif', 'for', 'foreach', 'while',
                      'do', 'try', 'catch', 'switch', 'finally',
                      'except', 'with'])
    matching_structures = set(['else', 'elif', 'catch', 'finally'])

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

    def __init__(self):
        super(LizardExtension, self).__init__(None)
        self.structure_piles = [0]  # Invariant: must always have at least one element

    def _push_scope(self):
        """Push a new scope level. Safe to call anytime."""
        self.structure_piles.append(0)

    def _pop_scope(self):
        """Pop a scope level. Maintains invariant of at least one element."""
        if len(self.structure_piles) > 1:
            self.structure_piles.pop()

    def _increment_current_scope(self):
        """Increment structure count in current scope. Safe even if piles corrupted."""
        if not self.structure_piles:
            self.structure_piles = [0]  # Restore invariant
        self.structure_piles[-1] += 1

    def _reset_or_decrement_current_scope(self, decrement=False):
        """Reset or decrement current scope counter. Safe even if piles corrupted."""
        if not self.structure_piles:
            self.structure_piles = [0]  # Restore invariant
            return
        if decrement:
            self.structure_piles[-1] -= 1
        else:
            self.structure_piles[-1] = 0

    def pile_up_within_block(self):
        self._increment_current_scope()
        cur_level = sum(self.structure_piles)
        # Is there a path around _state_global?
        if not hasattr(self.context.current_function, "max_nested_structures"):
            self.context.current_function.max_nested_structures = 0
        if self.context.current_function.max_nested_structures < cur_level:
            self.context.current_function.max_nested_structures = cur_level

    def _state_global(self, token):
        # Necessary to prevent functions without nested blocks to fail
        if not hasattr(self.context.current_function, "max_nested_structures"):
            self.context.current_function.max_nested_structures = 0
        if token == '{':
            self._push_scope()
        elif token in ';}':
            if token == '}':
                self._pop_scope()
            self._state = self._block_ending
        elif token in self.structures:
            self._state = self._in_structure_head

    @CodeStateMachine.read_inside_brackets_then("()")
    def _in_structure_head(self, token):
        self.pile_up_within_block()
        self._state = self._state_global
        self._state(token)

    def _block_ending(self, token):
        decrement = token in self.matching_structures
        self._reset_or_decrement_current_scope(decrement=decrement)
        self._state = self._state_global
        self._state(token)


def _init_nested_structure_data(self, *_):
    self.max_nested_structures = 0


patch_append_method(_init_nested_structure_data, FunctionInfo, "__init__")
