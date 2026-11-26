"""
Base class for all language parsers
"""

import re
from copy import copy
from itertools import compress

from .token_pattern import compiled_token_pattern, tokens_in_span


class CodeStateMachine:
    """ the state machine """
    # pylint: disable=R0903
    # pylint: disable=R0902
    # pylint: disable=E1102
    # disabled E1102 see https://github.com/PyCQA/pylint/issues/1493
    def __init__(self, context):
        self.context = context
        self.saved_state = self._state = self._state_global
        self.last_token = None
        self.to_exit = False
        self.callback = None
        self.rut_tokens = []
        self.br_count = 0

    def statemachine_clone(self):
        return self.__class__(self.context)

    def next(self, state, token=None):
        self._state = state
        if token is not None:
            return self(token)

    def next_if(self, state, token, expected):
        if token != expected:
            return
        self.next(state, token)

    def statemachine_return(self):
        self.to_exit = True
        self.statemachine_before_return()

    def fold_into_enclosing_function(self):
        '''The current function turned out not to be one: its lines and
        tokens count in the enclosing function instead.'''
        context = self.context
        dropped = context.current_function
        context.current_function = context.stacked_functions.pop()
        context.current_function.nloc += dropped.nloc
        context.current_function.token_count += dropped.token_count

    def sub_state(self, state, callback=None, token=None):
        self.saved_state = self._state
        self.callback = callback
        self.next(state, token)

    def __call__(self, token, reader=None):
        if self._state(token):
            self.next(self.saved_state)
            if self.callback:
                callback, self.callback = self.callback, None
                callback()
        self.last_token = token
        if self.to_exit:
            return True

    def _state_global(self, token):
        pass

    def statemachine_before_return(self):
        pass

    @staticmethod
    def read_inside_brackets_then(brs, end_state=None):
        def decorator(func):
            def read_until_matching_brackets(self, token):
                self.br_count += {brs[0]: 1, brs[1]: -1}.get(token, 0)
                if self.br_count == 0 or end_state is not None:
                    func(self, token)
                if self.br_count == 0 and end_state is not None:
                    self.next(getattr(self, end_state))
            return read_until_matching_brackets
        return decorator

    @staticmethod
    def read_until_then(tokens):
        def decorator(func):
            def read_until_then_token(self, token):
                if token in tokens:
                    func(self, token, self.rut_tokens)
                    self.rut_tokens = []
                else:
                    self.rut_tokens.append(token)
            return read_until_then_token
        return decorator


class MemberName(str):
    """A keyword that names a member, `promise.catch(f)`: no metric counts
    it as a structure."""

    is_member_name = True


# With a reader's conditions: the words that a metric counts as a
# structure or a jump.
_STRUCTURE_KEYWORDS = frozenset((
    'else', 'elif', 'do', 'repeat', 'switch', 'case', 'try', 'except',
    'finally', 'with', 'break', 'continue', 'goto', 'foreach', 'def'))
_COMMENT_STARTS = ('//', '/*')
_NOT_IN_TYPE_PARAMETERS = frozenset(('\n', '{', '}', ';', '='))
_NOT_IN_OPERATORS = frozenset(' \t\n()[]{},;:\'"`@#$\\_')
# Complete operators: in `a ..< .if` a member follows the range.
_RANGE_OPERATORS = frozenset(('...', '..<'))


def _is_operator(token):
    return not any(c.isalnum() or c in _NOT_IN_OPERATORS for c in token)


def _ends_dot_operator(tokens, index):
    """True if the `.` at `index` ends an operator that starts with a
    dot, such as `..` in `0..if` or a custom `.+.`."""
    start = index
    while index > 0 and _is_operator(tokens[index - 1]):
        index -= 1
    run = ''.join(tokens[index:start])
    return run[:1] == '.' and run not in _RANGE_OPERATORS


def _previous_code(tokens, index, line_breaks):
    """The index of the token before `index`, or -1. With `line_breaks`,
    line breaks and comments are skipped."""
    index -= 1
    while line_breaks and index >= 0 and (
            tokens[index] == '\n' or tokens[index].startswith(_COMMENT_STARTS)):
        index -= 1
    return index


def _before_type_parameters(tokens, index):
    """The index of the token before the `<...>` that ends at `index`,
    or -1."""
    depth = 0
    while index >= 0:
        token = tokens[index]
        if token == '>':
            depth += 1
        elif token == '<':
            depth -= 1
            if depth == 0:
                return index - 1
        elif token in _NOT_IN_TYPE_PARAMETERS:
            return -1
        index -= 1
    return -1


class CodeReader:
    """ CodeReaders are used to parse function structures from
    code of different
    language. Each language will need a subclass of CodeReader.  """

    ext = []
    languages = None
    extra_subclasses = set()

    # Condition categories - separate types that contribute to cyclomatic complexity
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    @classmethod
    def _build_conditions(cls):
        """Build combined conditions set from separated categories.

        Returns combined set of all condition types for CCN calculation.
        """
        return (cls._control_flow_keywords |
                cls._logical_operators |
                cls._case_keywords |
                cls._ternary_operators)

    def __init__(self, context):
        self.parallel_states = []
        self.context = context

        # Build combined conditions set from separated categories
        self.conditions = copy(self.__class__._build_conditions())

        # Expose individual categories for extensions
        self.control_flow_keywords = copy(self.__class__._control_flow_keywords)
        self.logical_operators = copy(self.__class__._logical_operators)
        self.case_keywords = copy(self.__class__._case_keywords)
        self.ternary_operators = copy(self.__class__._ternary_operators)

    @classmethod
    def match_filename(cls, filename):
        def compile_file_extension_re(*exts):
            return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.I)

        return compile_file_extension_re(*cls.ext).match(filename)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        pattern = compiled_token_pattern(addition)
        return tokens_in_span(
            pattern, source_code, 0, len(source_code), token_class)

    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            # Allow language-specific token processing
            if self.process_token(token):
                for state in self.parallel_states:
                    state(token)
                yield token
                continue

            for state in self.parallel_states:
                state(token)
            yield token
        for state in self.parallel_states:
            state.statemachine_before_return()
        self.eof()

    def eof(self):
        pass

    def mark_member_names(self, tokens, name_before, line_breaks=True,
                          type_parameters=False):
        """Drop the spaces but keep the line breaks, as the default
        preprocessing does. Mark a keyword after a token of `name_before`
        (a member access, or a keyword that declares a name) as the name
        of a member. With `line_breaks`, line breaks and comments in
        between are skipped. With `type_parameters`, so are type
        parameters after a declaring keyword: `fun <T> catch()`.
        Return a list."""
        keywords = (_STRUCTURE_KEYWORDS | self.control_flow_keywords
                    | self.case_keywords | self.logical_operators)
        tokens = [t for t in tokens if not t.isspace() or t == '\n']
        # Keywords are rare, so start from them and look back.
        for index in compress(range(len(tokens)), map(keywords.__contains__, tokens)):
            before = _previous_code(tokens, index, line_breaks)
            if type_parameters and before > 0 and tokens[before] == '>':
                before = _before_type_parameters(tokens, before)
                if before < 0:
                    continue
                before = _previous_code(tokens, before + 1, line_breaks)
                if before < 0 or not tokens[before].isalpha():
                    continue    # Only a declaring keyword: `fun <T> catch`
            if before >= 0 and tokens[before] in name_before and not (
                    tokens[before] == '.' and _ends_dot_operator(tokens, before)):
                tokens[index] = MemberName(tokens[index])
        return tokens

    def process_token(self, token):
        """Process a token before normal handling.
        Return True if the token has been handled specially and should skip normal processing.

        Args:
            token: The token being processed

        Returns:
            bool: True if the token should skip normal processing, False otherwise
        """
        return False
