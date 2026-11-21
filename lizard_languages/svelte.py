'''
Language parser for Svelte components

The script blocks are read as in a Vue component. The markup around them
becomes the script statements that it stands for: `{#if a}` is
`if ( a ) {`, `{#each a as b}` is `for ( a ; b ) {`, `{#await p}` is
`try { p ;` with `{:catch e}` as `} catch ( e ) {`, `{#snippet f(a)}` is
`function f ( a ) {`, and each expression in a tag, an attribute or a
directive is a statement of its own. Plain text, tags, static attributes,
comments and styles add no tokens.

The component itself is one more function, `(component)`: the code outside
every function and snippet, in the instance script and in the markup. The
code outside functions in `<script module>` is not part of it.
'''

import re
from html import unescape
from itertools import repeat

from lizard_core.function_info import FunctionInfo
from .js_style_regex_context import (
    _EXPRESSION_KEYWORDS, _MARK_CACHE, REGEX_LITERAL, REGEX_TOKEN)
from .javascript_expression import end_before_line_token
from .javascript_statements import ImpliedSemicolons
from .javascript_templates import LINE_COMMENT, _template_end
from .typescript import TypeScriptReader
from .vue import VueReader, _attributes

# In markup: a comment, a start or end tag, or a '{' that opens code.
_MARKUP_ITEM = re.compile(r'<!--|<(/?)([A-Za-z][^\s/>{]*)|\{')
# In a start tag: an expression, a quoted value, or the end of the tag.
_TAG_ITEM = re.compile(r'''\{|=\s*(["'])|>''')
_QUOTED_VALUE_ITEM = {'"': re.compile(r'\{|"'), "'": re.compile(r"\{|'")}
# In code: the units that can hold a brace as text, a template or a brace.
# A regular expression starts after punctuation, a keyword such as `return`,
# or an arrow. A keyword after a '.' is a member name.
_CODE_ITEM = re.compile(
    r'/\*.*?\*/|' + LINE_COMMENT
    + r'''|"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|''' + REGEX_TOKEN
    + r'|(?:(?<![\w$])(?P<word>'
    + '|'.join(sorted(_EXPRESSION_KEYWORDS | {'await'}))
    + r')|=>)\s*' + REGEX_LITERAL + r'|[`{}]', re.S)
# Spaces or one comment, skipped one at a time so that no match backtracks.
_SPACE_OR_COMMENT = re.compile(r'\s+|/\*.*?\*/|' + LINE_COMMENT, re.S)
_REGEX_LITERAL = re.compile(REGEX_LITERAL)
# The head of a block tag. `{/* note */ a}` and `{// note` start code.
_TAG_HEAD = re.compile(r'\s*([#:@]|/(?![/*]))(else\s+if\b|[\w$]*)')
# Elements whose content is text, and the tags that end it.
_RAW_TEXT_END = {name: re.compile(r'</' + name + r'\s*>')
                 for name in ('script', 'style')}
_OPENING = frozenset(('(', '[', '{', '${'))
_CLOSING = frozenset((')', ']', '}'))
_MEMBER_ACCESS = ('.', '?.')
# The zeros at the start of the number of a character reference.
_LEADING_ZEROS = re.compile(r'(&#[xX]?)0+(?=[0-9a-fA-F])')


class _Switch(str):
    """A place in the code tokens where the unit that owns the code outside
    functions can change. It looks like a comment, so the preprocess of the
    TypeScript reader and its lookahead pass it on, in order, without effect.
    """
    module = False          # the start of the module script
    typescript = False      # the start of a script in TypeScript


# The end of a script block.
_SCRIPT_END = _Switch('//')
# Like a line that starts a statement, the end of a script block ends the
# last statement of the script, so markup code after it cannot continue it.
_STATEMENT_START = 'script'
COMPONENT = '(component)'
_COMMENT_STARTS = ('//', '/*')


def _script_start(tag):
    """The switch marker of a script tag. The module script runs once for
    all instances of the component. As in the Svelte compiler, attribute
    names are case sensitive, and values can hold character references."""
    attributes = dict(_attributes(tag))
    marker = _Switch('//')
    marker.module = ('module' in attributes
                     or _text(attributes.get('context', '')) == 'module')
    marker.typescript = tag.lang in ('ts', 'tsx', 'typescript')
    return marker


def _text(value):
    """An attribute value with its character references decoded."""
    try:
        return unescape(_LEADING_ZEROS.sub(r'\1', value))
    except ValueError:      # a number too long for any character
        return value


def _enter(context, unit):
    """Make unit the owner of the code outside functions from here on."""
    if context.current_function is context.global_pseudo_function:
        context.current_function = unit
    context.global_pseudo_function = unit


class _Units:
    """The unit that owns the code outside functions: the component, or the
    pseudo function of the module script. The component starts at its first
    code and ends at its last code, which can be the end of a function in
    it. A line that another unit counts before the component counts for
    the component too, as for a nested function, and the same holds the
    other way."""

    def __init__(self, context):
        self.context = context
        self.module = context.global_pseudo_function
        self.component = FunctionInfo(COMPONENT, context.fileinfo.filename)
        self.component.nloc = self.component.token_count = 0
        self.owner = self.component
        self.region_has_code = False
        self.end_line = 0       # the last line of code of the component
        self.counted = {}       # the last line that each unit counts
        _enter(context, self.component)

    def switch(self, module, last_line, line):
        """Switch to the module script or to the component. last_line is
        the line of the last code before, and line the start of the last
        line with code: (its number, the NLOC of the owner before it).
        Returns whether the next code is the first code of a new region."""
        unit = self.module if module else self.component
        if unit is self.owner:
            return False
        self.end_region(last_line, line)
        self.owner = unit
        _enter(self.context, unit)
        return True

    def first_code(self, line, shared):
        """The first code of a region, on a line that the code before it
        has counted when shared."""
        owner = self.owner
        self.region_has_code = True
        if self.context.current_function is owner:
            if shared and self.counted.get(owner) != line:
                owner.nloc += 1
            if owner is self.component and not owner.start_line:
                owner.start_line = line

    def end_region(self, last_line, line):
        """The current region ends; last_line is the line of its last code.
        The owner counts last_line when its NLOC has grown by more lines
        than the code before last_line since the start of the line."""
        if self.region_has_code:
            owner = self.owner
            if owner.nloc - line[1] > last_line - line[0]:
                self.counted[owner] = last_line
            if owner is self.component:
                self.end_line = last_line
        self.region_has_code = False

    def finish(self, last_line, line, state):
        """At the end of the tokens: end the open function, report the
        component if it has code, and give the module script its place.
        As in a script, the functions that are still open around it are
        not reported."""
        context = self.context
        self.end_region(last_line, line)
        state.statemachine_before_return()
        component = self.component
        component.end_line = self.end_line
        if component.start_line and not context.forgive_global:
            context.fileinfo.function_list.append(component)
        context.current_function = self.module
        context.global_pseudo_function = self.module


class _Statements(ImpliedSemicolons):
    """The places where a statement ends without a ';'. The end of a script
    ends its last statement, as a ';' does, so the code after it cannot
    continue it."""
    # pylint: disable=R0903

    def __init__(self, context):
        super().__init__(context)
        self._script_ends = 0   # the script ends read so far

    def before(self, token):
        script_ends = self._context.script_ends
        if self._script_ends == script_ends:
            return super().before(token)
        self._script_ends = script_ends
        super().before(token)
        return True


def _code_end(source, pos, end):
    """The position of the '}' that ends the code at pos, or None."""
    depth = 1
    # A regular expression at the start of the code.
    match = _REGEX_LITERAL.match(source, _skip_spaces(source, pos, end), end)
    if match:
        pos = match.end()
    while True:
        match = _CODE_ITEM.search(source, pos, end)
        if match is None:
            return None
        token = match.group()
        pos = match.end()
        if match.group('word') and _after_dot(source, match.start()):
            pos = match.end('word')
        elif token == '{':
            depth += 1
        elif token == '}':
            depth -= 1
            if depth == 0:
                return match.start()
        elif token == '`':
            pos, expressions = _template_end(source, match.start())
            if expressions is None or pos > end:
                return None


def _skip_spaces(source, pos, end):
    """The position after the spaces and comments at pos."""
    while True:
        match = _SPACE_OR_COMMENT.match(source, pos, end)
        if match is None:
            return pos
        pos = match.end()


def _after_dot(source, pos):
    """Whether the word at pos is a member name: it follows a '.', across
    spaces and block comments, that does not end a number."""
    pos -= 1
    while pos >= 0:
        if source[pos].isspace():
            pos -= 1
        elif source.endswith('*/', 0, pos + 1):
            pos = source.rfind('/*', 0, pos - 1) - 1
            if pos < -1:
                return False
        else:
            break
    if pos < 0 or source[pos] != '.':
        return False
    digits = pos
    while digits > 0 and source[digits - 1].isdigit():
        digits -= 1
    return digits == pos or (
        digits > 0 and (source[digits - 1].isalnum() or source[digits - 1] in '_$'))


def _balanced(tokens):
    """The tokens, or only their newlines when the brackets do not match:
    an invalid expression must not open a scope for the code after it."""
    depth = 0
    for token in tokens:
        if token in _OPENING:
            depth += 1
        elif token in _CLOSING:
            depth -= 1
            if depth < 0:
                break
    if depth == 0:
        return tokens
    return [token for token in tokens if token == '\n']


def _split_at_last(tokens, words):
    """The index of the last word at depth 0 that is not a member name,
    or None."""
    depth = 0
    found = None
    previous = None
    for index, token in enumerate(tokens):
        if token in _OPENING:
            depth += 1
        elif token in _CLOSING:
            depth -= 1
        elif depth == 0 and token in words and previous not in _MEMBER_ACCESS:
            found = index
        if not token.isspace():
            previous = token
    return found


class _MarkupTokens:
    """The tokens of the markup between two script blocks."""
    # pylint: disable=R0903

    def __init__(self, source, start, end):
        self.source = source
        self.start = self.line = start
        self.end = end
        self.blocks = []
        self.tail = []

    def __iter__(self):
        source, end = self.source, self.end
        pos = self.start
        while True:
            match = _MARKUP_ITEM.search(source, pos, end)
            if match is None:
                break
            pos = match.end()
            name = match.group(2)
            if match.group() == '<!--':
                close = source.find('-->', pos, end)
                pos = end if close < 0 else close + 3
            elif name is None:
                pos = yield from self._tag(pos)
            elif match.group(1):
                close = source.find('>', pos, end)
                pos = end if close < 0 else close + 1
            else:
                pos = yield from self._start_tag(pos)
                if (name in _RAW_TEXT_END
                        and source[pos - 2:pos] != '/>'):
                    close = _RAW_TEXT_END[name].search(source, pos, end)
                    pos = end if close is None else close.start()
        yield from self._newlines(end)
        for block in reversed(self.blocks):
            if block != 'key':
                yield '}'

    def _start_tag(self, pos):
        """Reads the expressions of a start tag; returns where it ends."""
        source, end = self.source, self.end
        while True:
            match = _TAG_ITEM.search(source, pos, end)
            if match is None:
                return end
            pos = match.end()
            if match.group() == '>':
                return pos
            if match.group(1):
                value_item = _QUOTED_VALUE_ITEM[match.group(1)]
                while True:
                    match = value_item.search(source, pos, end)
                    if match is None:
                        return end
                    pos = match.end()
                    if match.group() != '{':
                        break
                    pos = yield from self._tag(pos, in_start_tag=True)
            else:
                pos = yield from self._tag(pos, in_start_tag=True)

    def _tag(self, pos, in_start_tag=False):
        """Reads the tag in braces that starts before pos; returns where
        it ends. As in the Svelte compiler, a brace that does not close
        holds the rest of the markup."""
        head = _TAG_HEAD.match(self.source, pos, self.end)
        if in_start_tag and head and head.group(1) != '@':
            # An attribute value is an expression: `{/x/.test(a)}`.
            head = None
        close = _code_end(self.source, head.end() if head else pos, self.end)
        if close is None:
            return self.end
        yield from self._newlines(pos)
        self.tail = []
        if head is None:
            yield from self._statement(pos, close)
        else:
            yield from self._block_tag(
                head.group(1) + ''.join(head.group(2).split()), head.end(),
                close)
        yield from self.tail
        return close + 1

    def _block_tag(self, kind, pos, close):
        # pylint: disable=R0912
        blocks = self.blocks
        top = blocks[-1] if blocks else None
        if kind == '#if':
            blocks.append('if')
            yield from self._head(('if', '('), pos, close, (')', '{'))
        elif kind == '#each':
            blocks.append('each')
            tokens = self._code(pos, close)
            separator = _split_at_last(tokens, ('as',))
            if separator is not None:
                tokens[separator] = ';'
            yield 'for'
            yield '('
            yield from tokens
            yield ')'
            yield '{'
        elif kind == '#await':
            blocks.append('await')
            yield from self._await(pos, close)
        elif kind == '#key':
            blocks.append('key')
            yield from self._statement(pos, close)
        elif kind == '#snippet':
            blocks.append('snippet')
            yield from self._head(('function',), pos, close, ('{',))
        elif kind == ':else' and top in ('if', 'each'):
            yield from self._head(('}', 'else', '{'), pos, close, ())
        elif kind == ':elseif' and top == 'if':
            yield from self._head(
                ('}', 'else', 'if', '('), pos, close, (')', '{'))
        elif kind == ':then' and top == 'await':
            yield from self._statement(pos, close)
        elif kind == ':catch' and top == 'await':
            yield from self._catch(self._code(pos, close))
        elif kind[0] == '/' and kind[1:] in blocks:
            while True:
                block = blocks.pop()
                if block != 'key':
                    yield '}'
                if block == kind[1:]:
                    break
        elif kind == '@const':
            yield 'const'
            yield from self._ended(self._code(pos, close), grouped=False)
        elif kind in ('@html', '@render', '@debug', '@attach'):
            yield from self._statement(pos, close)

    def _await(self, pos, close):
        tokens = self._code(pos, close)
        separator = _split_at_last(tokens, ('then', 'catch'))
        yield 'try'
        yield '{'
        if separator is None:
            yield from self._ended(tokens)
            return
        yield from self._ended(tokens[:separator])
        if tokens[separator] == 'then':
            yield from self._ended(tokens[separator + 1:])
        else:
            yield from self._catch(tokens[separator + 1:])

    @staticmethod
    def _catch(tokens):
        yield '}'
        yield 'catch'
        if any(not token.isspace() for token in tokens):
            yield '('
            yield from tokens
            yield ')'
        else:
            yield from tokens
        yield '{'

    def _head(self, before, pos, close, after):
        yield from before
        yield from self._code(pos, close)
        yield from after

    def _statement(self, pos, close):
        yield from self._ended(self._code(pos, close))

    @staticmethod
    def _ended(tokens, grouped=True):
        code = [token for token in tokens if not token.isspace()]
        if not code:
            yield from tokens
        elif grouped and (code[0] == '{'
                          or _split_at_last(code, (',',)) is not None):
            # An object literal, or a sequence such as the getter and
            # setter of a function binding: one expression.
            yield '('
            yield from tokens
            yield ')'
            yield ';'
        else:
            yield from tokens
            yield ';'

    def _code(self, pos, close):
        """The tokens of the code from pos to close, after the newlines of
        the tag head before it. The newlines and comments after the code
        go to the tail, which comes after the tokens that end the tag."""
        code = self.source[pos:close]
        newlines = self.source.count('\n', self.line, pos)
        self.line = close
        tokens = _balanced(list(TypeScriptReader.generate_tokens(code)))
        # The slice is new on each pass, so its memo would only grow.
        _MARK_CACHE.pop(('js', id(code)), None)
        last = len(tokens)
        while last and (tokens[last - 1].isspace()
                        or tokens[last - 1].startswith(('//', '/*'))):
            last -= 1
        self.tail = tokens[last:]
        return ['\n'] * newlines + tokens[:last]

    def _newlines(self, pos):
        newlines = self.source.count('\n', self.line, pos)
        self.line = pos
        return repeat('\n', newlines)


class SvelteReader(VueReader):
    # pylint: disable=R0903

    ext = ['svelte']
    language_names = ['svelte']
    implied_semicolons = _Statements

    @staticmethod
    def markup_tokens(source_code, start, end):
        if start:
            yield _SCRIPT_END
        yield from _MarkupTokens(source_code, start, end)

    @staticmethod
    def tag_tokens(tag):
        yield _script_start(tag)
        yield from repeat('\n', tag.count('\n'))

    def code_tokens(self, tokens):
        """The script tags are switch markers, so all tokens are code."""
        return tokens

    @staticmethod
    def get_comment_from_token(token):
        """A comment, or None. A switch marker is not a comment of the file,
        for an extension that reads the tokens before preprocess does."""
        if token.startswith(_COMMENT_STARTS) and token.__class__ is not _Switch:
            return token[2:]
        return None

    def preprocess(self, tokens):
        """The tokens of TypeScriptReader.preprocess without the switch
        markers. The code outside functions goes to the component, except
        in the module script."""
        context = self.context
        state = self.parallel_states[0]
        units = _Units(context)
        context.script_ends = 0     # the number of scripts that have ended
        newlines = 1        # since the last code token
        first = True        # the next code token is the first of a region
        # The start of the last line with code: its number, and the NLOC of
        # the owner before it.
        line = (1, 0)
        try:
            for token in super().preprocess(tokens):
                if token == '\n':
                    newlines += 1
                elif token.startswith(_COMMENT_STARTS):
                    if token.__class__ is _Switch:
                        if token is _SCRIPT_END:
                            end_before_line_token(
                                state, context.expression_arrows,
                                _STATEMENT_START)
                            context.script_ends += 1
                        else:
                            context.class_field_annotations = token.typescript
                        if units.switch(token.module,
                                        context.current_line - newlines, line):
                            first = True
                            line = (context.current_line, units.owner.nloc)
                        continue
                    # As comment_counter counts the lines of a comment.
                    newlines += len(token.splitlines()) - 1
                else:
                    if first:
                        first = False
                        units.first_code(context.current_line, not newlines)
                    if newlines:
                        line = (context.current_line, units.owner.nloc)
                        newlines = 0
                yield token
        except GeneratorExit:
            # A comment with "GENERATED CODE" ends the tokens early.
            units.finish(context.current_line - newlines, line, state)
            raise
        units.finish(context.current_line - newlines, line, state)
