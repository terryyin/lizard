'''
Language parser for JSX
'''

from .javascript import JavaScriptReader
from .typescript import JSTokenizer, Tokenizer
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression


class JSXMixin:
    '''Base mixin class for JSX/TSX shared functionality'''
    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
            r"|(?:\$\w+)" + \
            r"|(?:\<\/\w+\>)" + \
            r"|`.*?`"
        js_tokenizer = JSTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok

    def _expecting_func_opening_bracket(self, token):
        if token == '<':
            self.next(self._expecting_jsx)
            return
        super()._expecting_func_opening_bracket(token)

    def _expecting_jsx(self, token):
        if token == '>':
            self.next(self._expecting_func_opening_bracket)


class JSXReader(JavaScriptReader, JSXMixin):
    # pylint: disable=R0903

    ext = ['jsx']
    language_names = ['jsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        return JSXMixin.generate_tokens(source_code, addition, token_class)


class XMLTagWithAttrTokenizer(Tokenizer):
    def __init__(self):
        super(XMLTagWithAttrTokenizer, self).__init__()
        self.tag = None
        self.state = self._global_state
        self.cache = ['<']

    def process_token(self, token):
        self.cache.append(token)
        if not token.isspace():
            result = self.state(token)
            if result is not None:
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
            self.cache.append("}")
            self.sub_tokenizer = JSTokenizer()
            self.state = self._after_tag

    def _body(self, token):
        if token == "<":
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            self.cache.pop()
            return self.flush()

        if token.startswith("</"):
            self.stop()
            return self.flush()

        if token == '{':
            self.sub_tokenizer = JSTokenizer()
            return self.flush()


def isidentifier(token):
    try:
        return token.isidentifier()
    except AttributeError:
        return token.encode(encoding='UTF-8')[0].isalpha()
