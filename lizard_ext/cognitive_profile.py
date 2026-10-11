"""Which tokens count as which cognitive-complexity structures."""

from copy import copy

# The readers list the control-flow keywords that add cyclomatic complexity
# (``control_flow_keywords``); this is what those words mean here.  A word
# means the same in every language that has it, so a profile only adds what
# cyclomatic complexity does not count and no reader lists: ``switch``, ``do``,
# ``goto``, Kotlin's ``when`` ...  ``case_keywords`` are case labels, which
# add nothing; where ``case`` opens the statement (Ruby, PL/SQL, ST, Erlang)
# the profile says so.  ``logical_operators`` are used as they are.
_ROLES = {
    'ifs': frozenset(('if', 'unless', 'guard')),
    'else_ifs': frozenset(('elif', 'elsif', 'elseif', 'else if')),
    'loops': frozenset(('for', 'foreach', 'while', 'until')),
    'do_likes': frozenset(('do', 'repeat')),
    'switches': frozenset(('match',)),
    'catches': frozenset(('catch', 'except', 'rescue')),
    'gotos': frozenset(('goto',)),
}


class _Profile(object):  # pylint: disable=R0902,R0903
    """What a language adds to what its reader already knows."""

    # pylint: disable=R0913,R0914
    def __init__(self, ifs=(), else_ifs=(), loops=(), do_likes=(),
                 switches=(), catches=(), gotos=(), ignore=(),
                 jumps=('break', 'continue'), ternary=True,
                 paren_heads=True, newline_ends_statement=False,
                 nullable_types=False, rvalue_refs=False, lambda_style=None,
                 case_insensitive=False, nesting=True):
        # Structures the reader does not list; completed in ``for_reader``.
        self.ifs = frozenset(ifs)
        self.else_ifs = frozenset(else_ifs)
        self.loops = frozenset(loops)
        self.do_likes = frozenset(do_likes)       # body first, ``do {} while``
        self.switches = frozenset(switches)
        self.catches = frozenset(catches)
        self.gotos = frozenset(gotos)
        # Keywords the reader lists that are no structure of their own here
        # (Lua's ``until`` closes ``repeat``).
        self.ignore = frozenset(ignore)
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

    def for_reader(self, reader):
        """A copy with the roles of the reader's control-flow keywords added."""
        known = set(getattr(reader, 'control_flow_keywords', ())) - self.ignore
        if self.case_insensitive:
            known = set(word.lower() for word in known)
        profile = copy(self)
        for role, words in _ROLES.items():
            setattr(profile, role, getattr(self, role) | (known & words))
        return profile


# What no reader lists because it adds no cyclomatic complexity.
_C_LIKE = {'switches': ('switch',), 'do_likes': ('do',), 'gotos': ('goto',)}
# Blocks are not brace-delimited: increments without nesting penalty.
_FLAT = {'nesting': False, 'ternary': False, 'jumps': ()}

_PROFILES = {
    'cpp': _Profile(loops=('foreach',),         # Qt
                    rvalue_refs=True, lambda_style='cpp', **_C_LIKE),
    'objectivec': _Profile(loops=('foreach',), rvalue_refs=True, **_C_LIKE),
    'java': _Profile(lambda_style='arrow', **_C_LIKE),
    'csharp': _Profile(loops=('foreach',), lambda_style='arrow',
                       nullable_types=True, **_C_LIKE),
    'javascript': _Profile(newline_ends_statement=True, **_C_LIKE),
    'typescript': _Profile(newline_ends_statement=True, **_C_LIKE),
    'tsx': _Profile(newline_ends_statement=True, **_C_LIKE),
    'vue': _Profile(newline_ends_statement=True, **_C_LIKE),
    'php': _Profile(**_C_LIKE),
    'ttcn': _Profile(**_C_LIKE),
    'solidity': _Profile(catches=('catch',), **_C_LIKE),
    'go': _Profile(switches=('switch', 'select'), gotos=('goto',),
                   ternary=False, paren_heads=False,
                   newline_ends_statement=True),
    'kotlin': _Profile(switches=('when',), do_likes=('do',),
                       nullable_types=True, newline_ends_statement=True),
    'swift': _Profile(switches=('switch',), do_likes=('repeat',),
                      nullable_types=True, paren_heads=False,
                      newline_ends_statement=True),
    'rust': _Profile(switches=('match',), do_likes=('loop',),
                     ternary=False, paren_heads=False),
    'scala': _Profile(switches=('match',), newline_ends_statement=True),
    'perl': _Profile(catches=('catch',),         # Try::Tiny
                     gotos=('goto',), jumps=('last', 'next', 'redo')),
    'r': _Profile(ternary=False),               # ``switch()`` is a function
    'zig': _Profile(switches=('switch',), ternary=False),
    'ruby': _Profile(ifs=('unless',), switches=('case',), **_FLAT),
    'lua': _Profile(do_likes=('repeat',), gotos=('goto',), ignore=('until',),
                    **_FLAT),
    'erlang': _Profile(switches=('case', 'receive'), **_FLAT),
    'fortran': _Profile(else_ifs=('elseif',), switches=('select',),
                        gotos=('goto',), case_insensitive=True, **_FLAT),
    'plsql': _Profile(switches=('case',), gotos=('goto',),
                      case_insensitive=True, **_FLAT),
    'st': _Profile(switches=('case',), case_insensitive=True, **_FLAT),
    'tnsdl': _Profile(**_FLAT),
}

_PYTHON = _Profile(switches=('match',))

# Readers that put the function on lizard's nesting stack only once its
# body starts; for them declarations (``void f(T&& x)``) are skipped.
_GATED_LANGUAGES = frozenset(('cpp', 'java', 'csharp', 'objectivec', 'ttcn'))

_UNARY_OR_COALESCING = frozenset((
    'not', '!', 'orelse', '.NOT.', '.not.', '??'))

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
