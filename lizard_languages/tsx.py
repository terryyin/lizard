'''
Language parser for TSX/JSX

Uses TypeScriptStates (from typescript.py) for function detection.
Only overrides tokenization to handle JSX-specific syntax (<Component>, {expressions}).
'''

from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression
from .typescript import TypeScriptReader
from .typescript import JSTokenizer, Tokenizer


class TSXReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['tsx', 'jsx']
    language_names = ['tsx', 'jsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Add support for TypeScript type annotations in JSX
        addition = addition + \
            r"|(?:<[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:<\/[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:#\w+)" + \
            r"|(?:\$\w+)" + \
            r"|(?:<\/\w+>)" + \
            r"|(?:=>)" + \
            r"|`.*?`"
        js_tokenizer = TSXTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok


class TSXTokenizer(JSTokenizer):
    def __init__(self):
        super().__init__()

    def process_token(self, token):
        if token == "<":
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            return

        if token == "=>":
            # Special handling for arrow functions
            yield token
            return

        for tok in super().process_token(token):
            yield tok


class XMLTagWithAttrTokenizer(Tokenizer):
    def __init__(self):
        super(XMLTagWithAttrTokenizer, self).__init__()
        self.tag = None
        self.state = self._global_state
        self.cache = ['<']
        self._attr_expr_active = False

    def __call__(self, token):
        if self.sub_tokenizer:
            for tok in self.sub_tokenizer(token):
                yield tok
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
                if self._attr_expr_active:
                    # The TSXTokenizer consumed the closing '}' of a JSX
                    # attribute expression.  Inject ';' so the state machine
                    # properly closes any expression-body arrow function
                    # that was opened inside the attribute (e.g.
                    # onClick={() => handler()}).
                    self._attr_expr_active = False
                    yield ';'
            return
        for tok in self.process_token(token):
            yield tok

    def process_token(self, token):
        self.cache.append(token)
        if not token.isspace():
            result = self.state(token)
            if result is not None:
                if isinstance(result, list):
                    for tok in result:
                        yield tok
                else:
                    return result
        return ()

    def abort(self):
        self.stop()
        return self.cache

    def flush(self):
        tmp, self.cache = self.cache, []
        return [''.join(tmp)]

    def _global_state(self, token):
        if not isidentifier(token):
            return self.abort()
        self.tag = token
        self.state = self._after_tag

    def _after_tag(self, token):
        if token == '>':
            self.state = self._body
        elif token == "/":
            self.state = self._expecting_self_closing
        elif isidentifier(token):
            self.state = self._expecting_equal_sign
        else:
            return self.abort()

    def _expecting_self_closing(self, token):
        if token == ">":
            self.stop()
            return self.flush()
        return self.abort()

    def _expecting_equal_sign(self, token):
        if token == '=':
            self.state = self._expecting_value
        else:
            return self.abort()

    def _expecting_value(self, token):
        if token[0] in "'\"":
            self.state = self._after_tag
        elif token == "{":
            # TSXTokenizer handles brace-depth tracking and stops at the
            # matching '}'.  Transition straight to _after_tag so the next
            # attribute (or '>') is processed correctly once the sub-
            # tokenizer finishes.
            self.state = self._after_tag
            self.sub_tokenizer = TSXTokenizer()
            self._attr_expr_active = True

    def _body(self, token):
        # Abort if token can't be JSX body content — likely a type
        # annotation close: React.FC<Props> = (...) => {
        if token in ('=', '=>', ';', ')'):
            return self.abort()

        if token == "<":
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            self.cache.pop()
            return self.flush()

        if token.startswith("</"):
            self.stop()
            return self.flush()

        if token == '{':
            self.sub_tokenizer = TSXTokenizer()
            return self.flush()


def isidentifier(token):
    try:
        return token.isidentifier()
    except AttributeError:
        return token.encode(encoding='UTF-8')[0].isalpha()
