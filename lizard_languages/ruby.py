'''
Language parser for Ruby
'''

import re

from .rubylike import RubylikeReader, RubylikeStateMachine, _ends_value
from .js_style_regex_context import _REGEX_LITERAL_RE
from .js_style_regex_expression import js_style_regex_expression
from .ruby_documentation import may_have_documentation, pattern_for_fragment
from .source_documentation import tokens_ignoring_documentation
from .token_pattern import compiled_token_pattern, tokens_in_span


class MyToken(str):
    def __new__(cls, value, *_):
        val = value.group(0) if hasattr(value, 'group') else value
        return super(MyToken, cls).__new__(cls, val)

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start() if hasattr(value, 'start') else 0


class _HeredocCall(MyToken):
    def __init__(self, match):
        super().__init__(match)
        self.opener = MyToken(match['heredoc_opener'])
        self.opener.begin = match.start('heredoc_opener')
        start, end = match.span('heredoc_suffix')
        self.suffix_start = start - self.begin
        self.suffix_end = end - self.begin


class _HeredocLines(str):
    """Line breaks in a literal whose opener was already emitted."""


def _heredoc_token(match):
    if match['heredoc_suffix']:
        return _HeredocCall(match)
    return MyToken(match)


class _MethodName(str):
    """A keyword that names a method, `m.end(0)` or `def self.for(user)`:
    neither the reader nor a metric reads it as a structure."""

    is_member_name = True


# The words that the reader or a metric reads as a keyword.
_KEYWORDS = frozenset((
    'begin', 'end', 'do', 'then', 'def', 'class', 'module', 'it', 'not', 'in',
    'if', 'unless', 'else', 'elsif', 'elseif', 'elif', 'case', 'when',
    'while', 'until', 'for', 'foreach', 'and', 'or', 'rescue', 'ensure',
    'try', 'catch', 'finally', 'except', 'switch', 'with', 'return',
    'yield'))
_NAME_BEFORE = frozenset(('.', 'def'))
_RUBY_WORD_TOKENS = r"|\w+:|\$\w+|\.+|:?\@{0,2}\w+\??\!?"
_BRANCH_BOUNDARIES = frozenset(('then', 'elsif', 'else', 'when', 'in', 'ensure'))


def _paired(opening, closing):
    '''A % literal from its opening to its closing bracket. A pair of
    the same brackets nests, as in `%w(a (b) c)` and `%r{#{a}}`. Real code
    nests at most two levels deep; this allows three.'''
    opening, closing = re.escape(opening), re.escape(closing)
    body = r"(?:\\.|[^" + opening + closing + r"\\])*"
    for _ in range(3):
        body = (r"(?:\\.|[^" + opening + closing + r"\\]|" +
                opening + body + closing + r")*")
    return opening + body + closing


def _any_delimited(one_line, name, excluded=''):
    '''A bracket pair, or any other punctuation that closes itself, such
    as `%q@a@` or `%q#a#`, except the `excluded`. Such a delimiter ends
    the literal on its line, so a `%` in a regex that the reader misses,
    such as `/^%w/`, cannot run far. Only a `%r` regex with such a
    delimiter can span lines, as an `x` flag regex often does.'''
    pairs = "|".join(_paired(pair[0], pair[1])
                     for pair in ('{}', '()', '[]', '<>'))
    line_end = r"\n" if one_line else ""
    delimiter = "(?P=" + name + ")"
    other = ("(?P<" + name + r">[^\w\s(){}\[\]<>" + excluded + "])" +
             r"(?:\\.|(?!" + delimiter + r")[^\\" + line_end + "])*" +
             delimiter)
    return "(?:" + pairs + "|" + other + ")"


# A `%` right after an operand, as in `n%i==0`, is modulo. So is a bare
# `%=`, as in `a %= 2`.
_PERCENT_LITERAL = (r"(?<![\w)\]}])\%(?:[qQwWiIsx]" +
                    _any_delimited(True, 'percent_typed') +
                    r"|" + _any_delimited(True, 'percent_bare', '=') +
                    r"|r" + _any_delimited(False, 'percent_regex') +
                    r"[a-z]*)")


# A heredoc can be one token when the opener ends the statement line
# and its body contains only text or simple references. Keep a shift
# such as pages <<~mask separate.
_SIMPLE_HEREDOC = (
    r"(?:(?<!\\\n)(?<!\\\r\n)^[ \t]*|(?<==)[ \t]*|(?P<heredoc_call>(?<=[(,])[ \t]*))"
    r"(?P<heredoc_opener><<(?P<heredoc_indent>[-~])?"
    r"(?P<heredoc_quoted>')?(?P<heredoc_label>[A-Za-z_]\w*)(?(heredoc_quoted)'))"
    r"(?(heredoc_call)(?P<heredoc_suffix>"
    r"(?:[ \t]*,[ \t]*(?:\w+:[ \t]+)?[:@]?\w+)*[ \t]*[)\]]+))[ \t]*\r?\n"
    r"(?:(?!^(?(heredoc_indent)[ \t]*)(?P=heredoc_label)\r?$)"
    r"(?(heredoc_quoted)[^\n]*|(?:[^#\n]|#(?!\{)"
    r"|#\{(?:[A-Z]\w*(?:::[A-Z]\w*)*|[@$]?[a-z_]\w*)\})*)\n)*"
    r"^(?(heredoc_indent)[ \t]*)(?P=heredoc_label)\r?$"
)


_INTERPOLATION_MARKERS = re.compile(r"\\.|#\{", re.S)


class RubyStateMachine(RubylikeStateMachine):
    structure_depth = None
    ns_structures = frozenset(('if', 'for', 'while', 'do'))
    modifier_structure = False
    structure_peak = 0
    statement_structure_peak = 0

    def _state_global(self, token):
        if self.structure_depth is not None and (
                token == ';' or (
                    token in _BRANCH_BOUNDARIES and not getattr(token, 'is_member_name', False))
                or (self.is_newline() and self.last_token is not None
                    and _ends_value(self.last_token) and token not in ('.', '&', ')', ']', '}'))):
            self.statement_structure_peak = 0
        super()._state_global(token)

    def _start_structure(self, opener, callback, token, function_body):
        child = self.statemachine_clone()
        depth = 0 if function_body else self.structure_depth
        child.structure_depth = depth + (opener in self.ns_structures)
        if opener == 'do' and self._state == self._modifier_do:
            child.structure_depth += self.modifier_structure
        child._record_structure(child.structure_depth)

        def finish_block():
            if not function_body:
                self.statement_structure_peak = max(self.statement_structure_peak, child.structure_peak)
                self.structure_peak = max(self.structure_peak, child.structure_peak)
            if callback is not None:
                callback()

        self.sub_state(child, finish_block, token)

    def _count_loop_modifier(self, token):
        if self.structure_depth is not None and token in self.ns_structures:
            self._record_structure(self._modified_statement_depth() + 1)

    def _modified_statement_depth(self):
        return max(self.structure_depth, self.statement_structure_peak)

    def _record_structure(self, depth):
        self.structure_peak = max(self.structure_peak, depth)
        function = self.context.current_function
        function.max_nested_structures = max(function.max_nested_structures, depth)

    _loop_depth = False
    _loop_keyword = None

    def _start_block(self, opener, callback=None, token=None, function_body=False):
        if opener != 'def' and getattr(self.context, 'ruby_tracks_depth', False):
            callback = self._depth_block_callback(opener, callback)
        if self.structure_depth is None:
            super()._start_block(opener, callback, token, function_body)
        else:
            structure = self._loop_keyword if opener == 'loop' else opener
            self._start_structure(structure, callback, token, function_body)

    def _depth_block_callback(self, opener, callback):
        function = self.context.current_function
        owns_depth = self._loop_depth if opener == 'loop' else (
            opener in ('if', 'case') and self._take_pending_depth())
        entry_depth = function.nesting_depth - owns_depth
        parent_peak = function.ruby_block_peak
        statement_peak = function.ruby_statement_peak
        function.ruby_block_peak = function.nesting_depth
        function.ruby_statement_peak = 0

        def close_depth():
            completed_peak = function.ruby_block_peak
            function.ruby_block_peak = (
                None if parent_peak is None else max(parent_peak, completed_peak))
            function.ruby_statement_peak = statement_peak
            if completed_peak > entry_depth:
                function.ruby_statement_peak = max(statement_peak, completed_peak)
            if owns_depth:
                self.context.add_nd_condition(-1)
            if callback is not None:
                callback()

        return close_depth

    def _start_loop(self, opener, state):
        self._loop_keyword = opener
        self._loop_depth = (opener in ('while', 'for')
                            and getattr(self.context, 'ruby_tracks_depth', False)
                            and self._take_pending_depth())
        super()._start_loop(opener, state)

    def _take_pending_depth(self):
        # Keep a keyword's level before a brace or line break can close it.
        function = self.context.current_function
        if not function.levels_awaiting_body:
            return False
        function.levels_awaiting_body -= 1
        return True

    def _loop_modifier(self, token):
        if token == 'while':
            self._record_modified_block_depth()
        self._count_loop_modifier(token)

    def _read_modifier(self, token):
        self.modifier_structure = token in self.ns_structures
        if self.structure_depth is not None and self.modifier_structure:
            self._record_structure(self._modified_statement_depth() + 1)
        if token == 'if':
            self._record_modified_block_depth()
        super()._read_modifier(token)

    def _record_modified_block_depth(self):
        if getattr(self.context, 'ruby_tracks_depth', False):
            function = self.context.current_function
            if function.ruby_statement_peak:
                function.ruby_statement_peak += 1
                self.context.record_nd_peak(function.ruby_statement_peak)

    def _if(self, token):
        # A modifier predicate can open a block: `return if jobs.any? do`.
        if token == 'do' and not self.br_count and not getattr(token, 'is_member_name', False):
            self.next(self._modifier_do)
        else:
            if (token in ('if', 'while') and self.follows_value() and not self.br_count
                    and not getattr(token, 'is_member_name', False)
                    and getattr(self.context, 'ruby_tracks_depth', False)
                    and not self.context.get_condition_depth()):
                self._record_modified_block_depth()
            super()._if(token)

    def _finish_modifier_do(self):
        self.next(self._state_global)

    def _modifier_do(self, token):
        # A block with parameters or a body on the next line has a
        # distinct opening. Keep reading the predicate on other tokens.
        if self.is_newline() or token in ('|', '||', ';'):
            self._start_block('do', callback=self._finish_modifier_do, token=token)
        else:
            self.next(self._if, token)


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']
    STATE_MACHINE = RubyStateMachine

    def track_nested_structures(self):
        self.parallel_states[0].structure_depth = 0

    def nesting_semicolons(self, tokens):
        self.context.ruby_tracks_depth = True
        self.block_starts = ('{', '${')
        boundaries = _NestingSemicolons(self.context)
        return boundaries.read(tokens), boundaries

    def preprocess(self, tokens):
        """Drop spaces, and mark a keyword right after '.', '&.' or 'def'
        on the same line as the name of a method.  At the end of a line,
        call `context.at_line_end` once if the state machine set it."""
        keywords, name_before = _KEYWORDS, _NAME_BEFORE
        context = self.context
        context.at_line_end = None
        if getattr(context, 'ruby_tracks_depth', False):
            tokens = _mark_logical_assignments(tokens)
        name_follows = False
        tokens = _expand_regexp_interpolation(_expand_percent_interpolation(tokens))
        for token in tokens:
            if type(token) is _HeredocLines:
                # The opener is the literal's token. Its disjoint body
                # contributes physical lines after the call closes.
                for _ in token:
                    yield '\n'
                context.add_nloc(len(token))
                continue
            if token.isspace():
                if token != '\n':
                    continue
                if context.at_line_end is not None:
                    # The state machine has read the last token of the
                    # line.  The next line decides if the expression ends.
                    yield token
                    token = yield from self._after_line_end(tokens)
                    if token is None:
                        return
            if name_follows and token in keywords:
                name_follows = False
                yield _MethodName(token)
                continue
            name_follows = token in name_before
            yield token
        if context.at_line_end is not None:
            context.at_line_end()

    def _after_line_end(self, tokens):
        """Find the next token after blank and comment lines.  Unless it
        goes on with the expression, `.map` or `&.map`, call
        `context.at_line_end` first.  Then pass on the skipped lines, so
        that a comment such as `#lizard forgive` comes after the end."""
        context = self.context
        skipped = []
        token = None
        for token in tokens:
            if token.isspace() and token != '\n':
                continue
            if token == '\n' or self.get_comment_from_token(token) is not None:
                skipped.append(token)
                continue
            break
        else:
            token = None
        if token not in ('.', '&'):
            at_line_end, context.at_line_end = context.at_line_end, None
            at_line_end()
        yield from skipped
        return token

    @staticmethod
    @js_style_regex_expression(language='ruby')
    def generate_tokens(source_code, addition='', token_class=None):
        addition = (r"|\w+:|\$\w+|\.+|:?\@{0,2}\w+\??\!?"
                    r"|" + _PERCENT_LITERAL + addition)
        read_token = MyToken
        if "<<" in source_code:
            addition += "|" + _SIMPLE_HEREDOC
            read_token = _heredoc_token
        percent_strings = '%(' in source_code or '%Q(' in source_code
        bracket_stack = []
        source = source_code
        while source is not None:
            tokens = tokens_ignoring_documentation(
                source, addition, read_token, pattern_for_fragment(source, source is source_code),
                may_have_documentation(source))
            for token in tokens:
                if token == "{":
                    bracket_stack.append("{")
                elif token == "}":
                    if bracket_stack:
                        if bracket_stack.pop() == '#{':
                            source = '"'+source[token.begin + 1:]
                            yield token
                            break
                elif token.startswith('"'):
                    first, sep, _ = token.partition('#{')
                    if sep:
                        yield first + '"'
                        yield '${'  # because #will be regarded as comments
                        bracket_stack.append(sep)
                        source = source[token.begin + token.find(sep)+2:]
                        break
                if type(token) is _HeredocCall:
                    yield token.opener
                    yield from tokens_in_span(
                        compiled_token_pattern(addition), token,
                        token.suffix_start, token.suffix_end, MyToken)
                    yield _HeredocLines('\n' * token.count('\n'))
                else:
                    if percent_strings and token.startswith(('%(', '%Q(')) and '#{' in token:
                        token.percent_interpolation = addition
                    yield token
            else:
                source = None


def _expand_percent_interpolation(tokens):
    for token in tokens:
        addition = getattr(token, 'percent_interpolation', None)
        if addition is None:
            yield token
        else:
            yield from _percent_interpolation_tokens(token, addition)


def _percent_interpolation_tokens(token, addition):
    start = token.index('(') + 1
    return _interpolation_tokens(token, start, len(token) - 1, addition) or [token]


def _interpolation_tokens(token, start, end, addition):
    fragments = []
    pattern = compiled_token_pattern(r'|\#[^\n]*' + addition)

    while start < end:
        marker = next((match for match in _INTERPOLATION_MARKERS.finditer(token, start, end)
                       if match[0] == '#{'), None)
        if marker is None:
            break
        expression, closing = _interpolation_expression(token, marker.end(), end, pattern)
        if closing is None:
            return []
        fragments.extend(('"'+token[start:marker.start()]+'"', '${'))
        fragments.extend(expression)
        fragments.append(closing)
        start = closing.begin + 1

    if not fragments:
        return []
    fragments.append('"'+token[start:end]+'"')
    return fragments


def _interpolation_expression(token, start, end, pattern):
    parts = []
    braces = 0
    for current in tokens_in_span(pattern, token, start, end, MyToken):
        if current == '{':
            braces += 1
        elif current == '}':
            if not braces:
                return parts, current
            braces -= 1
        parts.append(current)
    return parts, None


_REGEXP_INTERPOLATION_MARKERS = re.compile(r"\\.|#\{[@$]?[A-Za-z_]\w*(?:::[A-Za-z_]\w*)*\}|#\{", re.S)


def _has_regexp_expression(text):
    return '#{' in text and any(marker[0] == '#{' for marker in _REGEXP_INTERPOLATION_MARKERS.finditer(text))


def _plain_heredoc_at(source, opener):
    start = opener
    while start > 0 and source[start-1] in ' \t':
        start -= 1
    match = re.compile(_SIMPLE_HEREDOC, re.M).match(source, start)
    if match is not None:
        parts = match.groupdict()
        if parts.get('heredoc_quote') == "'" or not _has_regexp_expression(parts.get('heredoc_body', '')):
            return match


def _expand_regexp_interpolation(tokens):
    addition = _RUBY_WORD_TOKENS + r"|" + _PERCENT_LITERAL
    for token in tokens:
        if (token.startswith('/') and _has_regexp_expression(token)
                and _REGEX_LITERAL_RE.fullmatch(token)):
            yield from _interpolation_tokens(token, 1, token.rfind('/'), addition) or [token]
        else:
            yield token


class _NestingSemicolons:
    """A Ruby statement normally ends at its line break."""

    def __init__(self, context):
        self.context = context
        self.before_token = False

    def read(self, tokens):
        previous = None
        for token in tokens:
            if previous == 'else':
                self.context.clear_prev_was_else()
            self.before_token = (self.context.newline and previous is not None
                                 and token not in ('.', '&')
                                 and (previous in ('do', 'then') or _ends_value(previous)))
            if (token == ';' or (self.before_token and token not in (')', ']', '}'))
                    or (token in _BRANCH_BOUNDARIES and not getattr(token, 'is_member_name', False))):
                self.context.current_function.ruby_statement_peak = 0
            previous = token
            yield token


def _mark_logical_assignments(tokens):
    for token in tokens:
        if token in ('&&', '||'):
            following = next(tokens, None)
            if following == '=' and token.begin + len(token) == following.begin:
                token.is_logical_assignment = True
            yield token
            if following is not None:
                yield following
        else:
            yield token
