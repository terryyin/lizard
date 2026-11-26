'''
Language parser for Ruby
'''

import re

from .rubylike import RubylikeReader, RubylikeStateMachine, _ends_value
from .js_style_regex_expression import js_style_regex_expression
from .ruby_documentation import may_have_documentation, pattern_for_fragment
from .source_documentation import tokens_ignoring_documentation


class MyToken(str):
    def __new__(cls, value, *_):
        val = value.group(0) if hasattr(value, 'group') else value
        return super(MyToken, cls).__new__(cls, val)

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start() if hasattr(value, 'start') else 0


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


class RubyStateMachine(RubylikeStateMachine):

    _loop_depth = False

    def _start_block(self, opener, callback=None, token=None):
        if not getattr(self.context, 'ruby_tracks_depth', False):
            super()._start_block(opener, callback, token)
            return

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

        super()._start_block(opener, close_depth, token)

    def _start_loop(self, opener, state):
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

    def _read_modifier(self, token):
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

    def _modifier_do(self, token):
        # A block with parameters or a body on the next line has a
        # distinct opening. Keep reading the predicate on other tokens.
        if self.is_newline() or token in ('|', '||', ';'):
            self.next(self._state_global)
            self._start_block('do', token=token)
        else:
            self.next(self._if, token)


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']
    STATE_MACHINE = RubyStateMachine

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
        for token in tokens:
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
        bracket_stack = []
        source = source_code
        while source is not None:
            tokens = tokens_ignoring_documentation(
                source, addition, MyToken, pattern_for_fragment(source, source is source_code),
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
                yield token
            else:
                source = None


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
