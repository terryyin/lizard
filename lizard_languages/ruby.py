'''
Language parser for Ruby
'''

from .rubylike import RubylikeReader
from .js_style_regex_expression import js_style_regex_expression
from .script_language import ScriptLanguageMixIn


class MyToken(str):
    def __new__(cls, value, *_):
        return super(MyToken, cls).__new__(cls, value.group(0))

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start()


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        def process_source(source, _, matcher):
            return ScriptLanguageMixIn.generate_common_tokens(
                source,
                r"|^\=begin|^\=end" +
                r"|\%[qQrw]?\{(?:\\.|[^\}\\])*?\}" +
                r"|\%[qQrw]?\[(?:\\.|[^\]\\])*?\]" +
                r"|\%[qQrw]?\<(?:\\.|[^\>\\])*?\>" +
                r"|\%[qQrw]?\((?:\\.|[^\>\\])*?\)" +
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
