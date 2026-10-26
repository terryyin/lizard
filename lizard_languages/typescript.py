'''
Language parser for TypeScript
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .javascript_bindings import JavaScriptBindings
from .typescript_declarations import TypeScriptTypeAnnotationStates
from .typescript_states import TypeScriptStates
from .javascript_templates import interpolation_code, template_tokens

# One token, so each ? in ?? is not its own ternary decision.
NULLISH_COALESCING = r"|(?:\?\?)"
OPTIONAL_ACCESS = r"|(?:\?\.(?!\d))"


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
        self._previous_token = None

    def process_token(self, token):
        # An interpolation is one token for the states, or a part of one in
        # TSX; the conditions written in it are counted here.
        for start, end in interpolation_code(
                token, self._previous_token == '${'):
            for inner in self.generate_tokens(token[start:end]):
                if inner in self.conditions:
                    self.context.add_condition()
        self._previous_token = token
        return False

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Private method (#), dollar ($), optional name (?), nullish (??).
        addition = (addition + r"|(?:#\w+)" + r"|(?:\$\w+)"
                    + r"|(?:\w+\?(?![?.]))" + NULLISH_COALESCING
                    + OPTIONAL_ACCESS)
        yield from template_tokens(source_code, addition, token_class)
