"""Hold uncertain function tokens and select per-function cognitive state."""

from itertools import chain
from weakref import WeakKeyDictionary


class _Candidate(object):  # pylint: disable=R0903
    """A function that the reader started at a ``(`` and can still abandon."""

    __slots__ = ('function', 'parent', 'depth')

    def __init__(self, function, parent, depth):
        self.function = function
        self.parent = parent    # the function around it
        self.depth = depth      # open brackets, with the ``(`` it started at


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


class _FunctionCandidates(object):  # pylint: disable=R0902
    """Track function candidates until the reader keeps or abandons them."""

    def __init__(self):
        self.states = WeakKeyDictionary()
        self.function = None
        self.state = None
        self.previous = None    # the token before this one
        self.previous_function = None   # and the function it arrived in
        self.candidates = []    # functions the reader can still abandon
        self.held = {}          # function -> lists of (order, token, line)
        self.order = 0          # number of tokens held so far

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
        self.state = self.states[function]

    def _in_body(self, function):
        return (self.context.last_function is function or
                function is self.context.global_pseudo_function)
