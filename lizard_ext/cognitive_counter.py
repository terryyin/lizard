"""Per-function cognitive-complexity state and shared increments."""

from weakref import WeakKeyDictionary

from .cognitive_profile import (
    _MEMBER_ACCESS, _SELF_REFERENCES, _UNARY_OR_COALESCING)

HEAD, PAREN_HEAD, BRACE_HEAD, BODY_PENDING, BODY = range(5)

_IF_LIKE = ('if', 'else if')
_EXPRESSION_FRAMES = ('ternary', 'lambda')


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

class _Counter(object):
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

    def process(self, token):
        function = self.context.current_function
        if function is not self.function:
            self._switch_function(function)
        state = self.state
        if self.gated and not state.body_started:
            if not self._in_body(function):
                return
            state.body_started = True
        if self.profile.case_insensitive:
            token = token.lower()
        self._process(token)
        state.prev2, state.prev = state.prev, token

    def _switch_function(self, function):
        self.function = function
        if function not in self.states:
            self.states[function] = _FunctionState()
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
        if token in self.profile.jumps:
            state.pending_jump_line = self.context.current_line
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
                    self.context.current_line == state.pending_jump_line:
                self._fundamental()
            state.pending_jump_line = None
            state.pending -= 1
