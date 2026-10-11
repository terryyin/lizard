'''
Language parser for TSX/JSX

Uses TypeScriptStates (from typescript.py) for function detection.
Only overrides tokenization to handle JSX-specific syntax (<Component>, {expressions}).
'''

from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression
from .typescript import TypeScriptReader, TEMPLATE_LITERAL
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
            r"|" + TEMPLATE_LITERAL
        js_tokenizer = TSXTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok


class TSXTokenizer(JSTokenizer):
    def __init__(self):
        super().__init__()

    def __call__(self, token):
        tag = self.sub_tokenizer
        for tok in super().__call__(token):
            if isinstance(tag, XMLTagWithAttrTokenizer) and tag.aborted:
                # A tag that turned out to be a comparison or type parameter
                # list hands its tokens back; their braces are this level's.
                self.depth += {'{': 1, '}': -1}.get(tok, 0)
            yield tok

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
        self._has_valued_attribute = False
        self.aborted = False

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
        if token.isspace() and '\n' in token:
            # Attribute expressions pass through before the cached tag text,
            # so newlines go out now to keep their lines counted in order.
            for _ in range(token.count('\n')):
                yield '\n'
            self.cache.append(' ')
            return
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
        self.aborted = True
        return self.cache

    def flush(self):
        tmp, self.cache = self.cache, []
        return [''.join(tmp)]

    def _global_state(self, token):
        if not isidentifier(token):
            return self.abort()
        self.tag = token
        self.state = self._after_tag_name

    def _after_tag_name(self, token):
        if token == '.':
            # Member tag name with attributes: <Modal.Footer className="x">
            self.state = self._tag_name_part
            return None
        if token.startswith('<') and token.endswith('>') and len(token) > 2:
            self.state = self._after_type_arguments
            return None
        return self._after_tag(token)

    def _after_type_arguments(self, token):
        # <ModalSetting<number> value={v} /> is an element; a generic call
        # such as useState<Result<T>>(x) has no attribute after them.
        if isidentifier(token) or token == '/':
            return self._after_tag(token)
        return self.abort()

    def _tag_name_part(self, token):
        if not isidentifier(token):
            return self.abort()
        self.state = self._after_tag_name
        return None

    def _after_tag(self, token):
        if token == '>':
            self.state = self._body
        elif token == "/":
            self.state = self._expecting_self_closing
        elif isidentifier(token):
            self.state = self._expecting_equal_sign
        elif token == "{":
            # Spread attribute: <Collapse {...props}>
            return self._start_expression()
        else:
            return self.abort()
        return None

    def _expecting_self_closing(self, token):
        if token == ">":
            self.stop()
            return self.flush()
        return self.abort()

    def _expecting_equal_sign(self, token):
        if token == '=':
            self._has_valued_attribute = True
            self.state = self._expecting_value
        elif token in ('-', ':'):
            # Hyphenated or namespaced name: data-action, xlink:href
            self.state = self._attribute_name_part
        elif token == '/' or self._has_valued_attribute:
            # Attribute without a value: <Icon fixedWidth />
            return self._after_tag(token)
        elif isidentifier(token):
            # <Modal show onHide={f}> or <T extends U>: only an `=` after the
            # second word shows the first was an attribute without a value.
            self.state = self._expecting_equal_sign_after_word
        else:
            return self.abort()
        return None

    def _expecting_equal_sign_after_word(self, token):
        if token == '=':
            return self._expecting_equal_sign(token)
        return self.abort()

    def _attribute_name_part(self, token):
        if not isidentifier(token):
            return self.abort()
        self.state = self._expecting_equal_sign

    def _expecting_value(self, token):
        if token[0] in "'\"":
            self.state = self._after_tag
        elif token == "{":
            # TSXTokenizer handles brace-depth tracking and stops at the
            # matching '}'.  Transition straight to _after_tag so the next
            # attribute (or '>') is processed correctly once the sub-
            # tokenizer finishes.
            self.state = self._after_tag
            self._attr_expr_active = True
            return self._start_expression()
        else:
            # `<T extends A = B>`: a type default, not an attribute value.
            return self.abort()
        return None

    def _start_expression(self):
        # The tag text read so far goes out before the expression, so the
        # state machine meets `<D f={` before the expression's tokens, and
        # an abort cannot hand back an opening '{' whose '}' the
        # expression's tokenizer consumes.
        self.sub_tokenizer = TSXTokenizer()
        return self.flush()

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
