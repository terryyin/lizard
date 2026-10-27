'''
Language parser for TypeScript
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .javascript_bindings import JavaScriptBindings
from .typescript_declarations import TypeScriptTypeAnnotationStates
from .typescript_states import TypeScriptStates
from .javascript_templates import template_tokens

# One token, so each ? in ?? is not its own ternary decision.
NULLISH_COALESCING = r"|(?:\?\?)"
OPTIONAL_ACCESS = r"|(?:\?\.(?!\d))"
# JavaScript identifiers can contain dollar signs as well as word characters.
JS_IDENTIFIER = r"|(?:[\w$]+)"


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
    _logical_operators = {'&&', '||', '??'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    def preprocess(self, tokens):
        """Drop spaces, and offer each line's first code token to the
        innermost open expression arrow before any counter charges it.

        A '\n' is consumed by line_counter, and comments by comment_counter,
        before the state machine sees them, so expression_arrows cannot
        change between a newline and the next code token."""
        arrows = self.context.expression_arrows = []
        line_start = False
        for token in self.code_tokens(tokens):
            if not token.isspace():
                if line_start and not token.startswith(('//', '/*')):
                    line_start = False
                    arrows[-1].end_expression_before(token)
                yield token
            elif token == '\n':
                if arrows:
                    line_start = True
                yield token

    @staticmethod
    def code_tokens(tokens):
        """The tokens that are code; embedding readers (Vue) narrow them."""
        return tokens

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Private method (#), dollar ($), optional name (?), nullish (??).
        addition = (addition + r"|(?:#\w+)"
                    + r"|(?:\w+\?(?![?.]))" + JS_IDENTIFIER + NULLISH_COALESCING
                    + OPTIONAL_ACCESS)
        yield from template_tokens(source_code, addition, token_class)
