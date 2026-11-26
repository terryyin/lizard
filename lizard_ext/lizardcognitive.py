"""
Cognitive Complexity: how hard a function is to *understand*.

Enable with ``lizard -Ecognitive``. It adds a ``cognitive_complexity`` value
(``CogC`` column) to every function, a ``--CogC`` warning threshold (default
15, as SonarSource uses), and it works with ``-s cognitive_complexity`` and
``-T cognitive_complexity=N`` like any other field.

The rules are SonarSource's "Cognitive Complexity" white paper by G. Ann
Campbell (specification v1.2, with the clarifications up to v1.7):

* Structural increment, +1 plus the current nesting level, for ``if``, the
  ternary operator, ``switch``, ``for``/``foreach``, ``while``/``do while``
  and ``catch``.  Each of them also nests its body one level deeper.
* Hybrid increment, +1 without a nesting penalty, for ``else`` and
  ``else if``/``elif``.  Their bodies are still nested one level deeper.
* Fundamental increment, +1 regardless of nesting, for every sequence of
  like binary logical operators (``a && b && c`` is one sequence,
  ``a && b || c`` is two), for ``goto``, for ``break``/``continue`` to a
  label or a number, and once for a function that calls itself.
* No increment for the function itself, ``try``/``finally``, ``case``
  labels, plain ``break``/``continue``/``return``, null-coalescing operators
  or method calls.  A lambda adds a nesting level without an increment.

Nesting is tracked from lizard's token stream for brace-delimited languages
(C, C++, Java, C#, Objective-C, JavaScript/TypeScript, Go, Rust, Kotlin,
Swift, Scala, PHP, Perl, R, Solidity, Zig, TTCN) and for indentation-based
ones (Python, GDScript).  For the remaining languages the structural and
fundamental increments are counted but the nesting penalty is not applied.
C preprocessor conditionals (``#if``, ``#ifdef``) are not counted, because
lizard's C reader resolves them before any extension sees the tokens.

The extension never changes the token stream or the language readers, so
enabling it leaves every other metric untouched.
"""
import sys

from lizard import FunctionInfo

from .cognitive_brace import _BraceCounter
from .cognitive_indent import _counter_for as _default_counter_for
from .cognitive_profile import _PROFILES

DEFAULT_COGNITIVE_THRESHOLD = 15


class LizardExtension(object):  # pylint: disable=R0903

    FUNCTION_INFO = {
        "cognitive_complexity": {
            "caption": " CogC ",
            "average_caption": " Avg.CogC "}}

    @staticmethod
    def set_args(parser):
        parser.add_argument(
            "--CogC",
            help='''Threshold for cognitive complexity warning.
            The default value is %d. Functions with cognitive complexity
            bigger than it will generate warning
            ''' % DEFAULT_COGNITIVE_THRESHOLD,
            type=int,
            dest="CogC",
            default=DEFAULT_COGNITIVE_THRESHOLD)

    def __call__(self, tokens, reader):
        _add_default_field(type(reader.context.global_pseudo_function))
        counter = _counter_for(reader)
        for token in tokens:
            counter.process(token)
            yield token
        counter.finish()


def _add_default_field(function_info_class):
    """Every function of the class reads ``cognitive_complexity`` as 0 until
    the counter increments it.  A class attribute covers the functions this
    extension never gets a token of: a Perl ``sub fwd;`` is created and
    finished within one step of the reader, between two tokens."""
    if 'cognitive_complexity' not in vars(function_info_class):
        function_info_class.cognitive_complexity = 0


def _add_default_everywhere():
    """``python lizard.py`` runs lizard.py as ``__main__`` and importing
    ``lizard`` above loads it a second time, so there are two FunctionInfo
    classes and only the ``__main__`` one is instantiated.  Patching
    ``__init__`` of the imported one would miss every function; the default
    goes on both copies (``__mp_main__`` is a multiprocessing worker's name
    for ``__main__``), and ``__call__`` repeats it on whatever class the
    context really uses."""
    _add_default_field(FunctionInfo)
    for module in ('__main__', '__mp_main__'):
        cls = getattr(sys.modules.get(module), 'FunctionInfo', None)
        if isinstance(cls, type):
            _add_default_field(cls)


_add_default_everywhere()


def _counter_for(reader):
    if getattr(reader, 'has_scala_interpolation', False):
        return _ScalaBraceCounter(reader, _PROFILES['scala'].for_reader(reader), gated=False)
    return _default_counter_for(reader)


class _ScalaBraceCounter(_BraceCounter):
    def __init__(self, reader, profile, gated):
        super().__init__(reader, profile, gated)
        self._interpolations = []

    def _process(self, token):
        if getattr(token, 'is_scala_interpolation', False):
            self._interpolation_boundary(token)
            return
        if getattr(token, 'is_scala_string_part', False):
            start_line = self.line - token.count('\n')
            if start_line != self.state.line:
                self._new_line(start_line)
            self.state.line = self.line
        super()._process(token)

    def _interpolation_boundary(self, token):
        state = self.state
        if token == '{':
            nesting = getattr(state, 'enclosing_interpolation_frames', 0)
            self._interpolations.append(
                (state, state.frames, state.depth, state.pdepth, state.logical, nesting))
            # A surrounding condition may still be reading its head. Keep its
            # nesting contribution and let the substitution read its own frames.
            state.enclosing_interpolation_frames = nesting + len(state.frames)
            state.frames = []
            state.depth = 0
            state.pdepth = 0
            state.logical = {}
        elif self._interpolations:
            owner, frames, depth, pdepth, logical, nesting = self._interpolations.pop()
            owner.enclosing_interpolation_frames = nesting
            owner.frames = frames
            owner.depth = depth
            owner.pdepth = pdepth
            owner.logical = logical
            # Line breaks inside the substitution belong to the string.
            owner.line = self.line

    def _structural(self):
        state = self.state
        self._add(1 + len(state.frames) + getattr(state, 'enclosing_interpolation_frames', 0))
