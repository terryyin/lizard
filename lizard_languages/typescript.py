'''
Language parser for TypeScript
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .javascript_bindings import JavaScriptBindings
from .typescript_declarations import TypeScriptTypeAnnotationStates
from .typescript_states import TypeScriptStates

# A template literal; quoted strings inside ${...} may contain backticks (#497).
TEMPLATE_LITERAL = (
    r"`(?:\\.|\$\{(?:\"(?:\\.|[^\"\\])*\"|'(?:\\.|[^'\\])*'"
    r"|[^{}\"'`])*\}|[^`\\])*`"
)


class Tokenizer(object):
    def __init__(self):
        self.sub_tokenizer = None
        self._ended = False

    def __call__(self, token):
        if self.sub_tokenizer:
            for tok in self.sub_tokenizer(token):
                yield tok
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
            return
        for tok in self.process_token(token):
            yield tok

    def stop(self):
        self._ended = True

    def process_token(self, token):
        pass


class JSTokenizer(Tokenizer):
    def __init__(self):
        super().__init__()
        self.depth = 1

    def process_token(self, token):
        if token == "{":
            self.depth += 1
        elif token == "}":
            self.depth -= 1
            if self.depth == 0:
                self.stop()
                return
        yield token


class TypeScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['ts']
    language_names = ['typescript', 'ts']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        def split_template_literal(token, quote):
            content = token[1:-1]

            # Always yield opening quote
            yield quote

            # If no expressions, yield content as-is with quotes and closing quote
            if '${' not in content:
                if content:
                    yield quote + content + quote
                yield quote
                return

            # Handle expressions
            i = 0
            while i < len(content):
                idx = content.find('${', i)
                if idx == -1:
                    if i < len(content):
                        yield quote + content[i:] + quote
                    break
                if idx > i:
                    yield quote + content[i:idx] + quote
                yield '${'
                i = idx + 2
                expr_start = i
                brace_count = 1
                while i < len(content) and brace_count > 0:
                    if content[i] == '{':
                        brace_count += 1
                    elif content[i] == '}':
                        brace_count -= 1
                    i += 1

                if brace_count > 0:
                    yield token
                    return

                expr = content[expr_start:i - 1]
                yield expr
                yield '}'
                content = content[i:]
                i = 0

            # Always yield closing quote
            yield quote

        # Private method (#), dollar ($), optional chaining (?), template literals
        addition = addition + r"|(?:#\w+)" + r"|(?:\$\w+)" + r"|(?:\w+\?)" + r"|" + TEMPLATE_LITERAL
        for token in CodeReader.generate_tokens(source_code, addition, token_class):
            if (
                isinstance(token, str)
                and token.startswith('`')
                and token.endswith('`')
                and len(token) > 1
            ):
                for t in split_template_literal(token, '`'):
                    yield t
                continue
            yield token
