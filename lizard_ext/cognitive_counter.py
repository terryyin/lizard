"""Per-function cognitive-complexity state and shared increments."""

from itertools import chain
from weakref import WeakKeyDictionary

from .cognitive_profile import (
    _MEMBER_ACCESS, _SELF_REFERENCES, _UNARY_OR_COALESCING)

HEAD, PAREN_HEAD, BRACE_HEAD, BODY_PENDING, BODY = range(5)

_IF_LIKE = ('if', 'else if')
_CLOSER_OF = {'(': ')', '[': ']', '{': '}', '${': '}'}
_BRACKETS = frozenset(_CLOSER_OF) | frozenset(_CLOSER_OF.values())
_EXPRESSION_FRAMES = ('ternary', 'lambda')


def _is_markup(token):
    """JSX text is no code: ``<p>{name}({count})</p>``."""
    return getattr(token, 'is_jsx_markup', False)


class _Candidate(object):  # pylint: disable=R0903
    """A function that the reader started at a ``(`` and can still abandon."""

    __slots__ = ('function', 'parent', 'depth')

    def __init__(self, function, parent, depth):
        self.function = function
        self.parent = parent    # the function around it
        self.depth = depth      # open brackets, with the ``(`` it started at


class _Frame(object):  # pylint: disable=R0903
    """An open control structure (or lambda / ternary) nesting its body."""

    __slots__ = ('kind', 'phase', 'braced', 'depth', 'pdepth', 'level',
                 'closing')

    def __init__(self, kind, phase, depth=0, pdepth=0, level=0):
        self.kind = kind
        self.phase = phase
        self.braced = False
        self.depth = depth      # brace depth of the body tokens
        self.pdepth = pdepth    # bracket depth where the frame was opened
        self.level = level      # lizard nesting level (indentation engine)
        self.closing = False    # body finished; waiting for else/catch/...


class _FunctionState(object):  # pylint: disable=R0902,R0903
    """Everything the counter remembers about one function."""

    def __init__(self):
        self.body_started = False
        self.frames = []
        self.depth = 0          # brace depth
        self.pdepth = 0         # parenthesis / bracket depth
        self.logical = {}       # pdepth -> last binary logical operator
        self.prev = None
        self.prev2 = None
        self.line = None
        self.continued = False
        self.after_async = False
        self.recursed = False
        self.pending = 0        # number of armed one-token lookaheads
        self.pending_ternary = 0
        self.pending_ternary_line = None
        self.pending_logical = None
        self.pending_logical_word = False
        self.pending_jump_line = None
        self.pending_call = False
        self.pending_ifs = []   # python: bracket depth of unclassified 'if's
        self.lambda_scan = None
        self.lambda_pdepth = 0
        self.do_tail = False


# ---------------------------------------------------------------------------
# Counters
# ---------------------------------------------------------------------------

class _Counter(object):  # pylint: disable=R0902
    """Bookkeeping per function plus the rules shared by all engines."""

    def __init__(self, reader, profile, gated):
        self.reader = reader
        self.context = reader.context
        self.profile = profile
        self.gated = gated
        self.logical_ops = frozenset(
            getattr(reader, 'logical_operators', ())) - _UNARY_OR_COALESCING
        self.states = WeakKeyDictionary()
        self.function = None
        self.state = None
        self.line = None
        self.openers = []       # (bracket, function) of each open bracket
        self.previous = None    # the token before this one
        self.previous_function = None   # and the function it arrived in
        self.candidates = []    # functions the reader can still abandon
        self.held = {}          # function -> lists of (order, token, line)
        self.order = 0          # number of tokens held so far

    def process(self, token, line=None, function=None):
        """Count the token for its function.  The held tokens come back with
        the line and the function they were read in."""
        if function is None:
            context = self.context
            line, function = context.current_line, context.current_function
            if self.profile.nested_functions:
                candidates = self.candidates
                if candidates or self.previous == '(':
                    self._settle()
                in_brackets = (candidates and
                               len(self.openers) >= candidates[-1].depth)
                read = token
                if token in _BRACKETS:
                    function, token = self._route(function, token)
                held = candidates and self._involved(function)
                if held:
                    self._hold(function, token)
                    top = candidates[-1]
                    if self._keeps_tokens(top, in_brackets, function, read):
                        candidates.pop()
                        self._release(top.function, top.parent)
                self.previous = read
                self.previous_function = context.current_function
                if held:
                    return
        if function is not self.function:
            self._switch_function(function)
        self.line = line
        state = self.state
        if self.gated and not state.body_started:
            if not self._in_body(function):
                return
            state.body_started = True
        if self.profile.case_insensitive:
            token = token.lower()
        self._process(token)
        state.prev2, state.prev = state.prev, token

    def finish(self):
        """The end of the file: count what is still held."""
        if self.held:
            self._settle()
            del self.candidates[:]
            self._release(*list(self.held))

    def _route(self, function, token):
        """The function that counts the bracket, and the token to count.

        A closing bracket belongs to the function that counted its opening
        bracket.  The reader can enter or leave a nested function between the
        two (``if (list.some(item => item.ok))``); the enclosing function
        would otherwise never see its bracket close.

        The ``}`` of a template interpolation ends no block.  It ends the
        sequence of logical operators before it, so it counts as ``,``."""
        openers = self.openers
        if token in _CLOSER_OF:
            if not _is_markup(token):
                openers.append((token, function))
        elif openers and _CLOSER_OF[openers[-1][0]] == token and \
                not _is_markup(token):
            opener, function = openers.pop()
            if opener == '${':
                token = ','
        return function, token

    # -- functions the reader abandons ----------------------------------------

    def _settle(self):
        """The reader takes ``x = (`` as the possible start of an arrow
        function, and abandons that function when no ``=>`` follows.  Its
        tokens then count for the function around it.  So the tokens of both
        functions are held from the ``(`` until it is known which function
        counts them."""
        context = self.context
        candidates = self.candidates
        while candidates and self._ended(candidates[-1].function):
            ended = candidates.pop()
            if getattr(ended.function, 'abandoned', False):
                self._merge(ended)
            self._release(ended.function, ended.parent)
        stacked = context.stacked_functions
        if self.previous == '(' and stacked and \
                stacked[-1] is self.previous_function and \
                context.current_function is not self.previous_function and \
                getattr(context.current_function, 'optimistic', False):
            candidates.append(_Candidate(
                context.current_function, stacked[-1], len(self.openers)))

    def _keeps_tokens(self, candidate, in_brackets, function, token):
        """Whether it is now known that the candidate counts its own tokens.

        After its brackets, ``=>`` or ``{`` starts its body: the reader keeps
        it.  In its brackets, ``=>``, ``function`` or a ``{`` that is not
        empty can be an arrow function, a function, a method or a class that
        the reader misses (``fn = (v => v && ok)``).  The tokens of such a
        function do not belong to the function around the candidate."""
        if in_brackets:
            return token in ('=>', 'function') or \
                (self.previous == '{' and token != '}')
        return token in ('=>', '{') and function is candidate.function

    def _ended(self, function):
        context = self.context
        if function is context.current_function:
            return False
        return not any(function is stacked
                       for stacked in context.stacked_functions)

    def _involved(self, function):
        return any(function is candidate.function or
                   function is candidate.parent
                   for candidate in self.candidates)

    def _hold(self, function, token):
        self.order += 1
        if function not in self.held:
            self.held[function] = [[]]
        self.held[function][0].append(
            (self.order, token, self.context.current_line))

    def _merge(self, candidate):
        function, parent = candidate.function, candidate.parent
        lists = self.held.pop(function, None)
        if lists:
            self.held.setdefault(parent, [[]]).extend(lists)
        # The brackets that the function opened: the ones above the ``(``
        # it started at, or in the place of that ``(`` after its ``)``.
        openers = self.openers
        for index in range(candidate.depth - 1, len(openers)):
            if openers[index][1] is function:
                openers[index] = (openers[index][0], parent)

    def _release(self, *functions):
        """Count the held tokens of the functions that no candidate involves
        any more, in the order in which they were read."""
        for function in functions:
            if function in self.held and not self._involved(function):
                lists = self.held.pop(function)
                tokens = lists[0] if len(lists) == 1 else \
                    sorted(chain.from_iterable(lists))
                for _, token, line in tokens:
                    self.process(token, line, function)

    def _switch_function(self, function):
        self.function = function
        if function not in self.states:
            self.states[function] = _FunctionState()
            # Non-zero scores land in __dict__ via +=; materialize 0 the same way.
            if 'cognitive_complexity' not in function.__dict__:
                function.cognitive_complexity = 0
        self.state = self.states[function]

    def _in_body(self, function):
        return (self.context.last_function is function or
                function is self.context.global_pseudo_function)

    def _process(self, token):
        raise NotImplementedError

    # -- increments -----------------------------------------------------------

    def _add(self, amount):
        self.function.cognitive_complexity += amount

    def _structural(self):
        self._add(1 + len(self.state.frames))

    def _hybrid(self):
        self._add(1)

    def _fundamental(self):
        self._add(1)

    # -- fundamental rules shared by all engines ------------------------------

    def _logical_operator(self, token):
        state = self.state
        if state.logical.get(state.pdepth) != token:
            self._fundamental()
            state.logical[state.pdepth] = token

    def _open_bracket(self):
        state = self.state
        state.pdepth += 1
        state.logical.pop(state.pdepth, None)

    def _close_bracket(self):
        state = self.state
        state.pdepth = max(0, state.pdepth - 1)
        for depth in [d for d in state.logical if d > state.pdepth]:
            del state.logical[depth]

    def _arm_word(self, token):
        """Remember identifiers that may need the next token to be judged:
        a call to the function itself and a ``break``/``continue`` label."""
        state = self.state
        function = self.function
        # The reader may still be appending to the name (``A`` -> ``A::f``),
        # so it is compared on every candidate token.
        if token in function.name and token == function.unqualified_name \
                and (state.prev not in _MEMBER_ACCESS or
                     (state.prev != '::' and state.prev2 in _SELF_REFERENCES)):
            state.pending_call = True
            state.pending += 1
        if token in self.profile.jumps and \
                not getattr(token, 'is_member_name', False):
            state.pending_jump_line = self.line
            state.pending += 1

    def _resolve_pending(self, token):
        state = self.state
        if state.pending_call:
            state.pending_call = False
            state.pending -= 1
            if token == '(' and not state.recursed:
                state.recursed = True
                self._fundamental()
        if state.pending_jump_line is not None:
            if token not in (';', '}') and \
                    self.line == state.pending_jump_line:
                self._fundamental()
            state.pending_jump_line = None
            state.pending -= 1
