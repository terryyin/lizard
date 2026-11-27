"""Bracket routing, control frames, and shared cognitive increments."""

from .cognitive_candidates import _FunctionCandidates
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


class _Counter(_FunctionCandidates):
    """Route tokens and apply the rules shared by all cognitive counters."""

    def __init__(self, reader, profile, gated):
        super(_Counter, self).__init__()
        self.reader = reader
        self.context = reader.context
        self.profile = profile
        self.gated = gated
        self.logical_ops = frozenset(
            getattr(reader, 'logical_operators', ())) - _UNARY_OR_COALESCING
        self.line = None
        self.openers = []       # (bracket, function) of each open bracket

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
        if token in function.name and token == function.unqualified_name:
            previous = state.prev
            # A grouped or type-asserted optional receiver could be `this`.
            if previous in _MEMBER_ACCESS or (
                    previous == '?.' and self.profile.optional_chaining
                    and state.prev2 not in (')', '!')):
                calls = previous != '::' and state.prev2 in _SELF_REFERENCES
            else:
                calls = getattr(token, 'is_member_name', False) or not (
                    self.profile.reserved_structures
                    and self._is_structure(token))
            if calls:
                state.pending_call = True
                state.pending += 1
        if token in self.profile.jumps and \
                not getattr(token, 'is_member_name', False):
            state.pending_jump_line = self.line
            state.pending += 1

    def _is_structure(self, token):
        profile = self.profile
        return (token in profile.ifs or token in profile.loops
                or token in profile.switches or token in profile.catches)

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
