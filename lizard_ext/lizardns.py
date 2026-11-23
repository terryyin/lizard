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
        # The tokens that _state_global reads; it skips the others.
        self._global_tokens = frozenset(self.structures) | frozenset((
            '{', ';', '}', 'else', '.', '?.'))

    def __call__(self, tokens, reader=None):
        if 'python' in getattr(reader, 'language_names', ()):
            return self._indented_structures(tokens, reader)
        if hasattr(reader, 'implied_semicolons'):
            return self._unterminated_statements(tokens, reader)
        return super(LizardExtension, self).__call__(tokens, reader)

    def _start_file(self, reader):
        super(LizardExtension, self)._start_file(reader)
        self.structure_piles = [0]

    def _unterminated_statements(self, tokens, reader):
        """JavaScript ends a statement without a ';' too: read a ';'
        where the reader finds such an end."""
        self._start_file(reader)
        tokens, implied_semicolons = reader.implied_semicolons(tokens)
        return self._read_with_implied_semicolons(tokens, implied_semicolons)

    def _read_with_implied_semicolons(self, tokens, implied_semicolons):
        for token in tokens:
            if implied_semicolons.before_token:
                self._state(';')
            self._state(token)
            yield token

    def _indented_structures(self, tokens, reader):
        """Python has no braces: a structure is a statement that starts a
        line, and its depth comes from the indentation, which the Python
        reader keeps as the nesting level of the context.

        For each function, the levels of the structures still open are kept
        in a list: a structure closes the open ones at its level or deeper.
        Structures inside brackets (comprehensions) or after the start of a
        statement (ternary 'x if c else y') are not structures here."""
        self.context = reader.context
        open_levels = {}
        brackets = 0
        last_line = None
        statement_start = True
        for token in tokens:
            line = self.context.current_line
            if line != last_line and brackets == 0:
                statement_start = True
            last_line = line
            if not hasattr(self.context.current_function,
                           "max_nested_structures"):
                self.context.current_function.max_nested_structures = 0
            if statement_start and token == 'async':
                yield token
                continue
            if statement_start and (
                    token in self.structures or
                    (token == 'match' and
                     getattr(reader, '_keyword_match', False))):
                self._open_indented_structure(open_levels)
            if token in ('(', '[', '{'):
                brackets += 1
            elif token in (')', ']', '}'):
                brackets = max(brackets - 1, 0)
            statement_start = False
            yield token

    def _open_indented_structure(self, open_levels):
        function = self.context.current_function
        level = self.context.current_nesting_level
        levels = open_levels.setdefault(id(function), [])
        while levels and levels[-1] >= level:
            levels.pop()
        levels.append(level)
        if function.max_nested_structures < len(levels):
            function.max_nested_structures = len(levels)

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
        if token not in self._global_tokens:
            return
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
            if getattr(token, 'is_member_name', False):
                pass                    # member/method name like catch or if
            elif token == 'else':
                self._state = self._after_else
            else:
                self._state = self._in_structure_head
        elif token in ('.', '?.'):
            self._state = self._member_name

    def _member_name(self, token):
        """A member named like a structure, promise.catch(f), is not one."""
        self._state = self._state_global
        if token not in self.structures:
            self._state(token)

    def _after_else(self, token):
        """An `else if` continues its chain as one structure."""
        self._state = self._in_structure_head
        if token != 'if':
            self._state(token)

    @CodeStateMachine.read_inside_brackets_then("()")
    def _in_structure_head(self, token):
        self.pile_up_within_block()
        self._state = self._state_global
        self._state(token)

    def _block_ending(self, token):
        # `} finally(e) {` in a class starts a method, not a clause.
        decrement = (token in self.matching_structures
                     and not getattr(token, 'is_member_name', False))
        self._reset_or_decrement_current_scope(decrement=decrement)
        self._state = self._state_global
        self._state(token)


def _init_nested_structure_data(self, *_):
    self.max_nested_structures = 0


patch_append_method(_init_nested_structure_data, FunctionInfo, "__init__")
