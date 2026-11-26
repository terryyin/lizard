'''
Language parser for Ruby
'''

import re

from lizard_core.function_info import FormalParameter

from .rubylike import RubylikeReader, RubylikeStateMachine
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


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [RubyStateMachine(context)]

    def preprocess(self, tokens):
        """Drop spaces, and mark a keyword right after '.', '&.' or 'def'
        on the same line as the name of a method.  At the end of a line,
        call `context.at_line_end` once if the state machine set it."""
        keywords, name_before = _KEYWORDS, _NAME_BEFORE
        context = self.context
        context.at_line_end = None
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


class RubyStateMachine(RubylikeStateMachine):
    _parameter_depth = 0
    _parameter_start = None
    _parameter_has_group = False

    def _def_parameters(self, token):
        if self._parameter_depth:
            self._read_grouped_parameter(token)
            return
        if token in (',', ')'):
            self._finish_parameter()
        else:
            if self._parameter_start is None:
                self._parameter_start = token
            if token in ('(', '[', '{', '${'):
                self._parameter_depth = 1
                self._parameter_has_group = True
        super()._def_parameters(token)

    def _read_grouped_parameter(self, token):
        if token in ('(', '[', '{', '${'):
            self._parameter_depth += 1
        elif token in (')', ']', '}'):
            self._parameter_depth -= 1

        if token == ',':
            self.context.add_to_long_function_name(' ,')
            self.context.current_function.full_parameters[-1] += ' ,'
        else:
            self.context.parameter(token)

    def _finish_parameter(self):
        parameters = self.context.current_function.full_parameters
        if self._parameter_start is not None:
            if self._parameter_has_group:
                name = self._parameter_start.rstrip(':')
                parameters[-1] = FormalParameter(
                    parameters[-1], name if name.isidentifier() else None)
            elif parameters[-1].strip() in ('*', '**', '&', '...'):
                parameters[-1] = FormalParameter(parameters[-1])
        self._parameter_start = None
        self._parameter_has_group = False
