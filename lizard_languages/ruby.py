'''
Language parser for Ruby
'''

from .rubylike import RubylikeReader
from .js_style_regex_expression import js_style_regex_expression
from .script_language import ScriptLanguageMixIn


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


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

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
        def process_source(source, _, matcher):
            return ScriptLanguageMixIn.generate_common_tokens(
                source,
                r"|^\=begin|^\=end" +
                r"|\%[qQrwiI]?\{(?:\\.|[^\}\\])*?\}" +
                r"|\%[qQrwiI]?\[(?:\\.|[^\]\\])*?\]" +
                r"|\%[qQrwiI]?\<(?:\\.|[^\>\\])*?\>" +
                r"|\%[qQrwiI]?\((?:\\.|[^\>\\])*?\)" +
                r"|\w+:" +
                r"|\$\w+" +
                r"|\.+" +
                r"|:?\@{0,2}\w+\??\!?" +
                _, matcher)
        matcher = MyToken
        bracket_stack = []
        source = source_code
        while source is not None:
            for token in process_source(source, addition, matcher):
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
