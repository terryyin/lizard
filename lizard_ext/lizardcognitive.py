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
from weakref import WeakKeyDictionary

from lizard import FunctionInfo
from lizard_ext.lizardnd import patch_append_method
from lizard_languages.python import PythonReader

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
        counter = _counter_for(reader)
        for token in tokens:
            counter.process(token)
            yield token


def _init_cognitive_complexity(self, *_):
    self.cognitive_complexity = 0


patch_append_method(_init_cognitive_complexity, FunctionInfo, "__init__")


# ---------------------------------------------------------------------------
# Language profiles: which tokens play which role
# ---------------------------------------------------------------------------

class _Profile(object):  # pylint: disable=R0902,R0903

    # pylint: disable=R0913,R0914
    def __init__(self, ifs=('if',), else_ifs=(), loops=('for', 'foreach', 'while'),
                 do_likes=('do',), switches=('switch',), catches=('catch',),
                 gotos=('goto',), jumps=('break', 'continue'), ternary=True,
                 paren_heads=True, newline_ends_statement=False,
                 nullable_types=False, rvalue_refs=False, lambda_style=None,
                 case_insensitive=False, nesting=True):
        self.ifs = frozenset(ifs)
        self.else_ifs = frozenset(else_ifs)
        self.loops = frozenset(loops)
        self.do_likes = frozenset(do_likes)       # body first, ``do {} while``
        self.switches = frozenset(switches)
        self.catches = frozenset(catches)
        self.gotos = frozenset(gotos)
        self.jumps = frozenset(jumps)             # count when followed by a label
        self.ternary = ternary
        # Heads are parenthesized (``if (...)``); otherwise a head runs until
        # the ``{`` of the body (Go, Rust, Swift).
        self.paren_heads = paren_heads
        # A newline ends a braceless body (languages without ``;``).
        self.newline_ends_statement = newline_ends_statement
        # ``T? x`` nullable type syntax competes with the ternary operator.
        self.nullable_types = nullable_types
        # C++ ``T&& x`` rvalue references compete with logical and.
        self.rvalue_refs = rvalue_refs
        # 'cpp': ``[...](...) {``;  'arrow': ``-> {`` or ``=> {``.
        self.lambda_style = lambda_style
        self.case_insensitive = case_insensitive
        # False: blocks are not brace-delimited, nesting is not tracked.
        self.nesting = nesting


_PROFILES = {
    'cpp': _Profile(rvalue_refs=True, lambda_style='cpp'),
    'objectivec': _Profile(rvalue_refs=True),
    'java': _Profile(lambda_style='arrow'),
    'csharp': _Profile(lambda_style='arrow', nullable_types=True),
    'javascript': _Profile(newline_ends_statement=True),
    'typescript': _Profile(newline_ends_statement=True),
    'tsx': _Profile(newline_ends_statement=True),
    'vue': _Profile(newline_ends_statement=True),
    'go': _Profile(loops=('for',), do_likes=(), switches=('switch', 'select'),
                   catches=(), ternary=False, paren_heads=False,
                   newline_ends_statement=True),
    'kotlin': _Profile(switches=('when',), gotos=(), nullable_types=True,
                       newline_ends_statement=True),
    'swift': _Profile(ifs=('if', 'guard'), do_likes=('repeat',), gotos=(),
                      nullable_types=True, paren_heads=False,
                      newline_ends_statement=True),
    'rust': _Profile(loops=('for', 'while'), do_likes=('loop',),
                     switches=('match',), catches=(), gotos=(),
                     ternary=False, paren_heads=False),
    'scala': _Profile(switches=('match',), gotos=(),
                      newline_ends_statement=True),
    'php': _Profile(else_ifs=('elseif',), switches=('switch', 'match')),
    'perl': _Profile(ifs=('if', 'unless'), else_ifs=('elsif',),
                     loops=('for', 'foreach', 'while', 'until'),
                     jumps=('last', 'next', 'redo')),
    'r': _Profile(loops=('for', 'while'), do_likes=('repeat',), switches=(),
                  catches=(), gotos=(), ternary=False),
    'zig': _Profile(loops=('for', 'while'), do_likes=(), gotos=(),
                    ternary=False),
    # Blocks are not brace-delimited: increments without nesting penalty.
    'ruby': _Profile(ifs=('if', 'unless'), else_ifs=('elsif',),
                     loops=('for', 'while', 'until'), do_likes=(),
                     switches=('case',), catches=('rescue',), gotos=(),
                     jumps=(), ternary=False, nesting=False),
    'lua': _Profile(else_ifs=('elseif',), loops=('for', 'while', 'repeat'),
                    do_likes=(), switches=(), catches=(), jumps=(),
                    ternary=False, nesting=False),
    'erlang': _Profile(loops=(), do_likes=(), switches=('case', 'receive'),
                       gotos=(), jumps=(), ternary=False, nesting=False),
    'fortran': _Profile(else_ifs=('elseif',), loops=('do',), do_likes=(),
                        switches=('select',), catches=(), jumps=(),
                        ternary=False, case_insensitive=True, nesting=False),
    'plsql': _Profile(else_ifs=('elsif',), loops=('for', 'while'),
                      do_likes=(), switches=('case',), catches=(), jumps=(),
                      ternary=False, case_insensitive=True, nesting=False),
    'st': _Profile(else_ifs=('elsif',), loops=('for', 'while', 'repeat'),
                   do_likes=(), switches=('case',), catches=(), gotos=(),
                   jumps=(), ternary=False, case_insensitive=True,
                   nesting=False),
    'tnsdl': _Profile(ternary=False, nesting=False),
}

_PYTHON = _Profile(else_ifs=('elif',), loops=('for', 'while'),
                   switches=('match',), catches=('except',))

# Readers that put the function on lizard's nesting stack only once its
# body starts; for them declarations (``void f(T&& x)``) are skipped.
_GATED_LANGUAGES = frozenset(('cpp', 'java', 'csharp', 'objectivec', 'ttcn'))

_UNARY_OR_COALESCING = frozenset(('not', '!', 'orelse', '.NOT.', '.not.'))

# After these the memory of the current logical operator sequence is
# dropped, so that ``a && b; c && d`` counts as two sequences.
_EXPRESSION_BREAKERS = frozenset((
    ';', ',', '{', '}', '?', ':', '=', '+=', '-=', '*=', '/=', '%=',
    '&=', '|=', '^=', '<<=', '>>=', 'return', 'yield'))

# ``?`` followed by one of these is not a ternary operator: ``?.``, ``??``,
# ``?:``, nullable types ``T?)``, Rust's ``foo()?;``, Java's ``? extends``.
_NOT_TERNARY_NEXT = frozenset((
    '?', '.', ':', ')', ']', '}', ',', ';', '=', '>', '<', '&&', '||',
    'extends', 'super'))

# ``T? x`` followed by one of these is a nullable type, not ``c ? x : y``.
_NULLABLE_TYPE_FOLLOWERS = frozenset((
    '=', ';', ',', ')', '{', '}', 'in', '=>', '>', ':'))

_MEMBER_ACCESS = frozenset(('.', '->', '::'))
_SELF_REFERENCES = frozenset(('this', 'self', 'cls'))
_LAMBDA_TRIGGERS = {'cpp': frozenset((']',)), 'arrow': frozenset(('->', '=>')),
                    None: frozenset()}


def _is_word(token):
    return bool(token) and (token[0].isalpha() or token[0] == '_')


# ---------------------------------------------------------------------------
# Per-function state
# ---------------------------------------------------------------------------

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
            if not hasattr(function, 'cognitive_complexity'):
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


class _BraceCounter(_Counter):  # pylint: disable=R0904
    """Nesting from braces, parentheses and semicolons (C-like languages).

    A structure is a frame: its *head* (``if (...)``) is followed by a body
    that is either braced or a single statement.  A finished body waits one
    token before its frame is popped, so that ``else``, ``catch``,
    ``finally`` and the ``while`` of a ``do`` bind to the right structure.
    """

    def __init__(self, reader, profile, gated):
        super(_BraceCounter, self).__init__(reader, profile, gated)
        self.lambda_triggers = _LAMBDA_TRIGGERS[profile.lambda_style]
        self.actions = self._keyword_actions(profile)
        if not profile.nesting:
            self._process = self._process_flat

    def _keyword_actions(self, profile):
        actions = {'{': self._open_block, '}': self._close_block,
                   ';': self._semicolon, 'else': self._else}
        for tokens, action in ((profile.ifs, self._if),
                               (profile.else_ifs, self._else_if),
                               (profile.loops, self._loop),
                               (profile.do_likes, self._do),
                               (profile.switches, self._switch),
                               (profile.catches, self._catch),
                               (profile.gotos, self._fundamental)):
            for token in tokens:
                actions[token] = action
        if profile.ternary:
            actions['?'] = self._question
        return actions

    def _process(self, token):  # pylint: disable=E0202
        state = self.state
        line = self.context.current_line
        if line != state.line:
            self._new_line(line)
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if state.pending_logical is not None:
            if self._resolve_pending_logical(token):
                return
        if state.pending_ternary:
            self._resolve_pending_ternary(token)
        frames = state.frames
        if frames and frames[-1].closing:
            if self._resolve_closing(token):
                return
        if token in self.logical_ops:
            if token == '&&' and self.profile.rvalue_refs:
                if state.lambda_scan != 'params':   # ``[](T&& t)``: a type
                    state.pending_logical = token
                    state.pending_logical_word = False
                return
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS:
            state.logical.pop(state.pdepth, None)
        if token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
            self._pop_expression_frames()
        if state.lambda_scan is not None or token in self.lambda_triggers:
            if self._scan_lambda(token):
                return
        if frames:
            top = frames[-1]
            if top.kind == 'ternary':
                top = self._top_structure()
            if top is not None and top.phase != BODY and \
                    self._in_head(top, token):
                return
        action = self.actions.get(token)
        if action is not None:
            action()

    # -- statement boundaries ---------------------------------------------------

    def _new_line(self, line):
        state = self.state
        state.line = line
        if not self.profile.newline_ends_statement or state.pdepth or \
                state.prev == ',':
            return
        if state.frames and not state.frames[-1].closing:
            top = state.frames[-1]
            if top.phase == BODY and not top.braced:
                self._statement_end()

    def _top_structure(self):
        for frame in reversed(self.state.frames):
            if frame.kind != 'ternary':
                return frame
        return None

    def _pop_expression_frames(self):
        """Ternary operators end with their enclosing brackets."""
        frames = self.state.frames
        while frames and frames[-1].kind == 'ternary' and \
                frames[-1].pdepth > self.state.pdepth:
            frames.pop()

    def _statement_end(self):
        """A ';' at bracket depth 0 finishes the braceless bodies around it."""
        state = self.state
        frames = state.frames
        while frames and frames[-1].kind == 'ternary':
            frames.pop()
        for frame in reversed(frames):
            if frame.braced or frame.depth != state.depth or \
                    frame.phase not in (BODY_PENDING, BODY):
                break
            frame.phase = BODY
            frame.closing = True

    def _block_closed(self):
        """A '}' has just brought the brace depth down to state.depth."""
        state = self.state
        frames = state.frames
        while frames and frames[-1].depth > state.depth:
            frame = frames[-1]
            if frame.braced and frame.depth == state.depth + 1 and \
                    frame.kind != 'lambda':
                frame.closing = True   # this '}' ends its body
                break
            frames.pop()               # lived inside the closed block
        if state.pdepth:
            return
        index = len(frames) - 1
        while index >= 0 and frames[index].closing:
            index -= 1
        while index >= 0:              # braceless bodies made of this block
            frame = frames[index]
            if frame.braced or frame.depth != state.depth or \
                    frame.phase != BODY:
                break
            frame.closing = True
            index -= 1

    def _resolve_closing(self, token):
        """Now that the token after a finished body is known, decide whether
        it continues that structure.  Returns True if the token is consumed."""
        state = self.state
        frames = state.frames
        bound = False
        if token == 'else':
            bound = self._bind(_IF_LIKE)
        elif token in self.profile.catches or token == 'finally':
            self._bind(('catch',))
            bound = True                # continues the enclosing try statement
        elif token == 'while' and self._bind(('do',)):
            state.do_tail = True
            bound = True
        if bound:
            for frame in frames:
                frame.closing = False
        else:
            while frames and frames[-1].closing:
                frames.pop()
        if bound and token == 'else':
            self._hybrid()
            self._push('else', BODY_PENDING)
            return True
        return False

    def _bind(self, kinds):
        """Pop the innermost finished frame of the given kinds, and the
        finished frames above it.  Returns False if there is none."""
        frames = self.state.frames
        for index in range(len(frames) - 1, -1, -1):
            if not frames[index].closing:
                return False
            if frames[index].kind in kinds:
                del frames[index:]
                return True
        return False

    def _in_switch_body(self):
        top = self._top_structure()
        return (top is not None and top.kind == 'switch' and
                top.phase == BODY and top.depth == self.state.depth)

    # -- structure heads and bodies ---------------------------------------------

    def _in_head(self, top, token):
        """Advance the head of the innermost structure; True if consumed."""
        state = self.state
        if top.phase == BODY_PENDING:
            return self._start_body(top, token)
        if top.phase == HEAD:
            if token == '(':
                top.phase = PAREN_HEAD
                top.pdepth = state.pdepth - 1
            elif token == '{' and state.pdepth == top.pdepth:
                self._open_braced_body(top)
            elif not self.profile.paren_heads:
                top.phase = BRACE_HEAD
            return True
        if top.phase == PAREN_HEAD:
            if token == ')' and state.pdepth == top.pdepth:
                top.phase = BODY_PENDING
            return True
        # BRACE_HEAD: Go / Rust / Swift ``if a && b {``
        if token == '{' and state.pdepth == top.pdepth:
            self._open_braced_body(top)
        return True

    def _start_body(self, top, token):
        if token == '{':
            self._open_braced_body(top)
            return True
        if top.kind == 'else' and token in self.profile.ifs:
            top.kind = 'else if'        # hybrid: already counted, no penalty
            top.phase = HEAD
            top.pdepth = self.state.pdepth
            return True
        top.phase = BODY
        top.braced = False
        top.depth = self.state.depth
        return False

    def _open_braced_body(self, frame):
        self.state.depth += 1
        frame.phase = BODY
        frame.braced = True
        frame.depth = self.state.depth

    def _push(self, kind, phase):
        state = self.state
        frame = _Frame(kind, phase, depth=state.depth, pdepth=state.pdepth)
        state.frames.append(frame)
        return frame

    # -- keyword actions ----------------------------------------------------------

    def _open_block(self):
        self.state.depth += 1

    def _close_block(self):
        self.state.depth = max(0, self.state.depth - 1)
        self._block_closed()

    def _semicolon(self):
        if not self.state.pdepth:
            self._statement_end()

    def _if(self):
        self._structural()
        self._push('if', HEAD)

    def _else_if(self):
        self._hybrid()
        self._push('else if', HEAD)

    def _else(self):
        if self._in_switch_body():
            return                      # Kotlin ``when { else -> }``
        frames = self.state.frames
        while frames and frames[-1].kind == 'ternary':
            frames.pop()
        # ``if (a) b else c`` without ';' (Kotlin, Scala, R ...)
        if frames and frames[-1].kind in _IF_LIKE and \
                frames[-1].phase == BODY and not frames[-1].braced:
            frames.pop()
        self._hybrid()
        self._push('else', BODY_PENDING)

    def _loop(self):
        if self.state.do_tail:
            self.state.do_tail = False  # the ``while`` of ``do {} while``
        else:
            self._structural()
            self._push('loop', HEAD)

    def _do(self):
        self._structural()
        self._push('do', BODY_PENDING)

    def _switch(self):
        self._structural()
        self._push('switch', HEAD)

    def _catch(self):
        self._structural()
        self._push('catch', HEAD)

    def _question(self):
        state = self.state
        if state.prev not in ('<', ',', '?'):
            state.pending_ternary = 1
            state.pending_ternary_line = self.context.current_line

    # -- one and two token lookaheads ----------------------------------------------

    def _resolve_pending_ternary(self, token):
        state = self.state
        if state.pending_ternary == 1:
            state.pending_ternary = 0
            if token in _NOT_TERNARY_NEXT:
                return
            if self.profile.nullable_types:
                if self.context.current_line != state.pending_ternary_line:
                    return
                if _is_word(token):     # ``T? x`` or ``c ? x : y``
                    state.pending_ternary = 2
                    return
        else:
            state.pending_ternary = 0
            if token in _NULLABLE_TYPE_FOLLOWERS:
                return
        self._structural()
        self._push('ternary', BODY)

    def _resolve_pending_logical(self, token):
        """C++: ``T&& x = ...`` and ``auto&& x : v`` are not logical ands."""
        state = self.state
        if not state.pending_logical_word and _is_word(token):
            state.pending_logical_word = True
            return True
        operator = state.pending_logical
        state.pending_logical = None
        if not (state.pending_logical_word and token in ('=', ':')):
            self._logical_operator(operator)
        state.pending_logical_word = False
        return False

    def _scan_lambda(self, token):
        """Detect lambda bodies, which nest without an increment."""
        state = self.state
        if state.lambda_scan is None:
            if token == ']':
                state.lambda_scan = 'after_capture'
            elif not self._in_switch_body():        # ``case 1 -> {`` is not
                state.lambda_scan = 'after_params'
            return False
        if state.lambda_scan == 'after_capture':
            if token == '(':
                state.lambda_scan = 'params'
                state.lambda_pdepth = state.pdepth - 1
                return False
            state.lambda_scan = None
            if token == '{':
                self._open_lambda()
                return True
            return False
        if state.lambda_scan == 'params':
            if token == ')' and state.pdepth == state.lambda_pdepth:
                state.lambda_scan = 'after_params'
            return False
        # after_params: ``mutable``, ``noexcept``, ``-> T`` ... then ``{``
        if token == '{':
            state.lambda_scan = None
            self._open_lambda()
            return True
        if token in (';', ',', ')', ']', '}', '?', ':', '(', '='):
            state.lambda_scan = None
        return False

    def _open_lambda(self):
        self._open_braced_body(self._push('lambda', BODY))

    # -- languages without brace-delimited blocks -------------------------------

    def _process_flat(self, token):
        state = self.state
        profile = self.profile
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if token in self.logical_ops:
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS:
            state.logical.pop(state.pdepth, None)
        if token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
        if state.pending_ternary:
            state.pending_ternary = 0
            if token not in _NOT_TERNARY_NEXT:
                self._structural()
        if token in profile.ifs:
            if state.prev != 'else':    # ``ELSE IF`` is one hybrid increment
                self._structural()
        elif token in profile.else_ifs or token == 'else' or \
                token in profile.loops or token in profile.do_likes or \
                token in profile.switches or token in profile.catches:
            self._structural()
        elif token in profile.gotos:
            self._fundamental()
        elif token == '?' and profile.ternary:
            state.pending_ternary = 1


class _IndentCounter(_Counter):
    """Nesting from indentation, through the nesting level lizard's Python
    reader derives from it (Python, GDScript)."""

    def _process(self, token):
        state = self.state
        if token == '\\\n':
            state.continued = True
            return
        statement_start = self._track_newline()
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if token in self.logical_ops:
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS or token in ('if', 'else', 'lambda'):
            state.logical.pop(state.pdepth, None)
        if token in ('(', '[', '{'):
            self._open_bracket()
        elif token in (')', ']', '}'):
            self._close_bracket()
            frames = state.frames
            while frames and frames[-1].kind in _EXPRESSION_FRAMES and \
                    frames[-1].pdepth > state.pdepth:
                frames.pop()
            while state.pending_ifs and state.pending_ifs[-1] > state.pdepth:
                state.pending_ifs.pop()    # comprehension filter: no increment
        if statement_start:
            self._statement_keyword(token)
        else:
            self._expression_keyword(token)

    def _track_newline(self):
        """Close the blocks the indentation closed; True at a statement start."""
        state = self.state
        line = self.context.current_line
        new_line = line != state.line and not state.continued
        state.continued = False
        state.line = line
        statement_start = state.after_async
        state.after_async = False
        if new_line and not state.pdepth:
            level = self.context.current_nesting_level
            frames = state.frames
            while frames and (frames[-1].kind in _EXPRESSION_FRAMES or
                              frames[-1].level >= level):
                frames.pop()
            state.logical.clear()
            statement_start = True
        return statement_start

    def _statement_keyword(self, token):
        profile = self.profile
        if token == 'async':
            self.state.after_async = True
        elif token in profile.ifs or token in profile.loops or \
                token in profile.catches or \
                (token in profile.switches and self._match_is_keyword()):
            self._structural()
            self._push(token)
        elif token in profile.else_ifs or token == 'else':
            self._hybrid()
            self._push(token)

    def _expression_keyword(self, token):
        state = self.state
        if token == 'if':
            if state.pdepth:
                state.pending_ifs.append(state.pdepth)   # ternary or comprehension
            else:
                self._structural()
                self._push_expression('ternary')
        elif token == 'else' and state.pending_ifs and \
                state.pending_ifs[-1] == state.pdepth:
            state.pending_ifs.pop()
            self._structural()
            self._push_expression('ternary')
        elif token == 'for' and state.pending_ifs and \
                state.pending_ifs[-1] == state.pdepth:
            state.pending_ifs.pop()     # ``[x for x in y if c for z in w]``
        elif token == 'lambda':
            self._push_expression('lambda')

    def _match_is_keyword(self):
        return getattr(self.reader, '_keyword_match', True)

    def _push(self, kind):
        self.state.frames.append(
            _Frame(kind, BODY, level=self.context.current_nesting_level))

    def _push_expression(self, kind):
        self.state.frames.append(_Frame(kind, BODY, pdepth=self.state.pdepth))


def _counter_for(reader):
    if isinstance(reader, PythonReader):
        return _IndentCounter(reader, _PYTHON, gated=True)
    name = reader.language_names[0]
    return _BraceCounter(reader, _PROFILES.get(name, _Profile()),
                         gated=name in _GATED_LANGUAGES)
