'''
Language reader for Twig templates.

The Twig compiler makes a method of each ``{% macro %}`` and each
``{% block %}``, and one more of the rest of the template. So each macro and
each block is a function here. The rest of the template is the function
``(template)`` when it has an output tag or a Twig tag other than the
declarations ``extends``, ``use``, ``import``, ``from``, ``block`` and
``macro``. Tags that Twig does not define (``trans``, ``sw_extends``) make
no ``(template)`` and add no decision point; their expressions count.

Text outside the Twig delimiters is no code. Like HTML outside
``<?php ... ?>`` for the PHP reader, it reaches the counters as string
tokens: one token for each run of nonblank text lines.

The delimiters become the punctuation of a C-like statement, so the counters
and the extensions read ``{% if a %}x{% else %}y{% endif %}`` as
``{% if a { "x" } else { "y" } endif ;``: ``%}`` of ``if``, ``elseif``,
``else`` and ``for`` is ``{``, ``{%`` of ``elseif``, ``else``, ``endif`` and
``endfor`` is ``}``, and the other closing delimiters are ``;``. The
braces of a hash are ``[`` and ``]``, and a word that is a keyword elsewhere
but a name here (``item.if``, ``{% include 'a' with b %}``) is quoted.
'''

import re

from .code_reader import CodeReader, CodeStateMachine


class _Name(str):
    '''A name that the metrics must not read as a keyword, ``item.if``,
    ``{for: 1}``, ``{% include 'a' with b %}``. Its text is quoted like a
    Twig hash key, and ``name`` keeps the word.'''

    def __new__(cls, name):
        token = str.__new__(cls, "'" + name + "'")
        token.name = name
        return token


class _TagName(str):
    '''The word after ``{%``. ``name`` keeps the word when the text is
    quoted.'''

    def __new__(cls, name, quoted=False):
        quoted = quoted or name in _FOREIGN
        token = str.__new__(cls, "'" + name + "'" if quoted else name)
        token.name = name
        return token


_OPENING = re.compile(r'\{([{%#])[-~]?')
_CLOSING = {'{': re.compile(r'[-~]?\}\}'), '%': re.compile(r'[-~]?%\}')}
_VERBATIM = re.compile(r'\s*(verbatim|raw)\s*[-~]?%\}')
_VERBATIM_END = {
    'verbatim': re.compile(r'\{%[-~]?\s*endverbatim\s*[-~]?%\}'),
    'raw': re.compile(r'\{%[-~]?\s*endraw\s*[-~]?%\}'),
}
# A run of nonblank lines, or a newline.
_TEXT = re.compile(r'\S[^\n]*(?:\n[^\S\n]*\S[^\n]*)*|\n')
_SPACE = re.compile(r'[^\S\n]+')
# A Twig name starts with a letter, '_' or a character above '\x7e'. The
# negated ASCII classes compile much faster than a range up to U+10FFFF.
_NAME_START = r'[^\x00-\x40\x5b-\x5e\x60\x7b-\x7e]'
_EXPRESSION = re.compile(
    r"b-(?:and|or|xor)(?!\w)"
    r"|" + _NAME_START + r"[^\x00-\x2f\x3a-\x40\x5b-\x5e\x60\x7b-\x7e]*"
    r"|\d[\d_]*(?:\.\d[\d_]*)?(?:[eE][+-]?\d+)?"
    r"|'(?:\\.|[^'\\])*'"
    r"|\.\.\.|\.\.|\?\.|\?[ \t]*:|\?\?|===|!==|==|!=|<=>|<=|>=|=>|\*\*|//"
    r"|.", re.S)
# The literal part of a double-quoted string, up to an interpolation.
_STRING_PART = re.compile(r'(?:\\.|[^"\\#]|#(?!\{))*', re.S)
_KEYWORDS = frozenset(('if', 'elseif', 'else', 'for', 'and', 'or'))
_OPENERS = {'(': ')', '[': ']', '{': '}'}
_CLOSERS = frozenset(_OPENERS.values())
_STRUCTURES = frozenset(('if', 'for'))
# Only the tag name and the condition of ``for ... in ... if`` use these as
# keywords. Anywhere else they are names.
_TAG_KEYWORDS = frozenset(('if', 'elseif', 'else', 'for'))
# Words that -ENS reads as structures in every language but that Twig has
# none of: the tags ``do`` and ``with``, ``include ... with``, variables.
_FOREIGN = frozenset(('elif', 'foreach', 'while', 'do', 'try', 'catch',
                      'switch', 'finally', 'except', 'with'))
# Names that -Eexitcount and -Egotocount read as keywords: ``call.return``.
# The ``{% return %}`` tag of Shopware scripts stays an exit.
_FOREIGN_NAMES = _FOREIGN | frozenset(('return', 'goto'))


def _text_tokens(text):
    for match in _TEXT.finditer(text):
        run = match.group(0)
        yield run if run == '\n' else '"' + run + '"'


class _ExpressionLexer:  # pylint: disable=R0903
    '''Reads the tokens of one tag, up to its closing delimiter at bracket
    depth 0. Strings, hashes and interpolations may hold the delimiter.'''

    def __init__(self, source, closing):
        self.source = source
        self.closing = closing
        self.tokens = []
        self.nesting = []   # open brackets, '"' strings and '#{' parts
        self.previous = None

    def read(self, pos):
        '''Returns the position after the tag, or None at the end of the
        source.'''
        source, end = self.source, len(self.source)
        while pos < end:
            if self.nesting and self.nesting[-1] == '"':
                pos = self._string_part(pos)
                continue
            match = _SPACE.match(source, pos)
            if match:
                pos = match.end()
                continue
            if self._may_close():
                match = self.closing.match(source, pos)
                if match:
                    return match.end()
            if source[pos] == '#':
                # An inline comment runs to the end of the line.
                newline = source.find('\n', pos)
                pos = end if newline < 0 else newline
                continue
            match = _EXPRESSION.match(source, pos)
            pos = match.end()
            self._token(match.group(0))
        return None

    def _may_close(self):
        '''Twig rejects a tag with an unclosed bracket. Reading on would
        take the rest of the template into it, so the delimiter ends the
        tag unless it can close a hash or an interpolation.'''
        nesting = self.nesting
        return not nesting or self.closing is _CLOSING['%'] or \
            ('{' not in nesting and '#{' not in nesting)

    def _string_part(self, pos):
        source = self.source
        match = _STRING_PART.match(source, pos)
        self.previous = '"' + match.group(0) + '"'
        self.tokens.append(self.previous)
        pos = match.end()
        if source.startswith('#{', pos):
            self.nesting.append('#{')
            return pos + 2
        self.nesting.pop()
        return pos + 1

    def _token(self, token):
        nesting = self.nesting
        if token == '\n':
            self.tokens.append(token)
            return
        if token == '"':
            nesting.append('"')
            return
        if token[0] == '?' and token[-1] == ':':
            token = '?:'
        elif token == '_self':
            token = 'self'     # the template itself: ``_self.macro()``
        elif token == ':' and self.previous == '?':
            # ``?`` and ``:`` on two lines: Twig reads ``?:`` too.
            index = len(self.tokens) - 1 - self.tokens[::-1].index('?')
            self.tokens[index] = self.previous = '?:'
            return
        if token in _OPENERS:
            nesting.append(token)
        elif token in _CLOSERS and nesting:
            if nesting.pop() == '#{':
                return
        elif token in _KEYWORDS:
            if self.previous in ('.', '?.'):
                token = _Name(token)
        elif token in (':', '=') and self.previous in _KEYWORDS:
            self.tokens[-1] = _Name(self.previous)
        if token in ('{', '}'):
            # A hash literal: data like an array, no block.
            token = '[' if token == '{' else ']'
        self.previous = token
        self.tokens.append(token)


def _tag_tokens(tokens, structures):
    '''The tag name and the C-like opening and closing tokens of a tag.'''
    words = [token for token in tokens if token != '\n']
    name = words[0] if words else ''
    opening, closing = '{%', ';'
    top = structures[-1] if structures else None
    if name in _STRUCTURES:
        structures.append(name)
        closing = '{'
    elif name == 'guard':
        structures.append(name)     # its ``else`` is no branch
    elif name == 'endguard' and top == 'guard':
        structures.pop()
    elif (name == 'elseif' and top == 'if') or \
            (name == 'else' and top in _STRUCTURES):
        opening, closing = '}', '{'
    elif name in ('endif', 'endfor') and top == name[3:]:
        structures.pop()
        opening = '}'
    if words:
        position = tokens.index(name)
        tokens[position] = _TagName(name, name == 'else' and top == 'guard')
        _names(tokens, position + 1, name)
    return opening, closing


def _names(tokens, start, tag):
    '''Marks the words that are names here.'''
    in_seen = False
    for position in range(start, len(tokens)):
        token = tokens[position]
        if token in _TAG_KEYWORDS and not (in_seen and token == 'if'):
            tokens[position] = _Name(token)
        elif token in _FOREIGN_NAMES:
            tokens[position] = _Name(token)
        elif token == 'in' and tag == 'for':
            in_seen = True
        elif token in _KEYWORDS and position == start and \
                tag in ('block', 'endblock', 'macro'):
            tokens[position] = _Name(token)


def _calls(tokens, units):
    '''A filter, or a function called in a block, is not the block or macro
    of the same name: ``{% block head %}{{ head() }}``.'''
    if not units:
        return
    kinds = dict((name, kind) for kind, name in units)
    previous = None
    for position in range(len(tokens) - 1):
        token = tokens[position]
        if token in kinds and tokens[position + 1] == '(' and \
                (previous == '|' or kinds[token] == 'block' and
                 previous not in ('.', '?.')):
            tokens[position] = _Name(token)
        previous = token


def _track_units(tokens, units):
    words = [token for token in tokens if token != '\n']
    tag = words[0] if words else ''
    if tag in ('block', 'macro') and len(words) > 1 and \
            (tag == 'macro' or len(words) == 2):    # not ``{% block a 'x' %}``
        units.append((tag, getattr(words[1], 'name', words[1])))
    elif tag in ('endblock', 'endmacro') and units and \
            units[-1][0] == tag[3:]:
        units.pop()


def generate_twig_tokens(source):
    structures = []
    units = []      # kind and name of each open block and macro
    pos = 0
    while True:
        match = _OPENING.search(source, pos)
        if not match:
            yield from _text_tokens(source[pos:])
            return
        yield from _text_tokens(source[pos:match.start()])
        kind = match.group(1)
        pos = match.end()
        if kind == '#':
            end = source.find('#}', pos)
            end = len(source) if end < 0 else end + 2
            yield source[match.start():end]
            pos = end
            continue
        verbatim = _VERBATIM.match(source, pos) if kind == '%' else None
        if verbatim:
            yield from _verbatim_tokens(source, verbatim)
            end = _VERBATIM_END[verbatim.group(1)].search(source, verbatim.end())
            pos = len(source) if end is None else end.end()
            continue
        lexer = _ExpressionLexer(source, _CLOSING[kind])
        pos = lexer.read(pos)
        _calls(lexer.tokens, units)
        if kind == '{':
            opening, closing = '{{', ';'
            _names(lexer.tokens, 0, None)
        else:
            opening, closing = _tag_tokens(lexer.tokens, structures)
            _track_units(lexer.tokens, units)
        yield opening
        yield from lexer.tokens
        if pos is None:
            return
        yield closing


def _verbatim_tokens(source, verbatim):
    word = verbatim.group(1)
    end = _VERBATIM_END[word].search(source, verbatim.end())
    opening = verbatim.group(0)
    yield '{%'
    yield from '\n' * opening.count('\n', 0, verbatim.start(1) - verbatim.start())
    yield _TagName(word)
    yield from '\n' * opening.count('\n', verbatim.end(1) - verbatim.start())
    yield ';'
    content = source[verbatim.end():end.start() if end else len(source)]
    yield from _text_tokens(content)
    if end:
        newlines = end.group(0).count('\n')
        yield '{%'
        yield _TagName('end' + word)
        for _ in range(newlines):
            yield '\n'
        yield ';'


class TwigReader(CodeReader):
    '''Reader for Twig templates, ``*.twig``.'''

    ext = ['twig']
    language_names = ['twig']

    _control_flow_keywords = {'if', 'elseif', 'for'}
    _logical_operators = {'and', 'or'}
    _case_keywords = set()
    _ternary_operators = {'?'}
    # What -END opens a level for (its default adds while, try, case ...).
    loops = frozenset(('if', 'for', '?'))

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TwigStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return generate_twig_tokens(source_code)

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('{#'):
            return token[2:].lstrip('-~')
        return None


# The tags of Twig, other than the declarations extends, use, import,
# from, block and macro. Tags that Twig does not define add nothing.
_CODE_TAGS = frozenset((
    'apply', 'autoescape', 'cache', 'deprecated', 'do', 'embed', 'flush',
    'for', 'guard', 'if', 'include', 'sandbox', 'set', 'types', 'verbatim',
    'with', 'raw', 'spaceless', 'filter'))


class TwigStates(CodeStateMachine):
    '''Starts a function at each block and macro and ends it at its end
    tag. The function (template) holds the rest.'''

    def __init__(self, context):
        super().__init__(context)
        context.restart_new_function('(template)')
        self.template = context.current_function
        self.template.nloc = 0
        self.template.token_count = 0
        self.template_has_code = False
        self.units = []     # 'block' or 'macro' for each open function
        self.depth = 0
        self._state = self._first_token

    def _first_token(self, token):
        self.template.start_line = \
            self.context.current_line - token.count('\n')
        self.next(self._state_global, token)

    def _state_global(self, token):
        if isinstance(token, _TagName):
            tag = token.name
            if not self.units and tag in _CODE_TAGS:
                self.template_has_code = True
            if tag == 'block':
                self.next(self._block_name)
            elif tag == 'macro':
                self.next(self._macro_name)
            elif tag in ('endblock', 'endmacro') and \
                    self.units and self.units[-1] == tag[3:]:
                self.next(self._end_tag)
        elif token == '{{' and not self.units:
            self.template_has_code = True

    def _start(self, kind, token):
        name = getattr(token, 'name', token)
        if re.match(_NAME_START, name):
            self.context.push_new_function(name)
            self.units.append(kind)
            return True
        self.next(self._state_global, token)
        return False

    def _block_name(self, token):
        if self._start('block', token):
            self.next(self._block_head)

    def _block_head(self, token):
        if token == ';':
            self.next(self._state_global)
        else:
            self.next(self._end_tag)

    def _macro_name(self, token):
        if self._start('macro', token):
            self.next(self._macro_head)

    def _macro_head(self, token):
        if token == '(':
            self.depth = 1
            self.next(self._macro_parameters)
        else:
            self.next(self._state_global, token)

    def _macro_parameters(self, token):
        if token == ';':
            self.next(self._state_global)
            return
        if token in _CLOSERS:
            self.depth -= 1
            if self.depth == 0:
                self.next(self._state_global)
                return
        if self.depth == 1:
            self.context.parameter(getattr(token, 'name', token))
        if token in _OPENERS:
            self.depth += 1

    def _end_tag(self, token):
        if token == ';':
            self._end_unit()
            self.next(self._state_global)

    def _end_unit(self):
        self.units.pop()
        self.context.end_of_function()

    def statemachine_before_return(self):
        while self.units:
            self._end_unit()
        context = self.context
        # -Eoutside has ended the current function already.
        if context.current_function is self.template:
            if self.template_has_code:
                context.end_of_function()
            else:
                context.current_function = context.global_pseudo_function
