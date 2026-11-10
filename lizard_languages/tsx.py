'''
Language parser for TSX/JSX

Uses TypeScriptStates (from typescript.py) for function detection.
Only overrides tokenization to handle JSX-specific syntax (<Component>, {expressions}).
'''

from .js_style_regex_expression import js_style_regex_expression
from .typescript import (
    JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, OPTIONAL_NAME,
    TypeScriptReader)
from .typescript import JSTokenizer, Tokenizer
from .javascript_templates import LINE_COMMENT, template_tokens
from .javascript_functions import JSXChildBrace


class TSXReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['tsx', 'jsx']
    language_names = ['tsx', 'jsx']
    _scan_atomic_templates = True

    def __init__(self, context):
        super().__init__(context)
        context.class_field_annotations = context.fileinfo.filename.lower().endswith(
            ".tsx")

    @staticmethod
    @js_style_regex_expression(language='jsx')
    def generate_tokens(source_code, addition='', token_class=None):
        # Add support for TypeScript type annotations in JSX
        addition = addition + "|" + LINE_COMMENT + \
            r"|(?:<[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:<\/[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:#\w+)" + \
            OPTIONAL_NAME + \
            JS_IDENTIFIER + \
            NULLISH_COALESCING + \
            OPTIONAL_ACCESS + \
            r"|(?:<\/\w+>)" + \
            r"|(?:<\/?>)" + \
            r"|(?:=>)"
        js_tokenizer = TSXTokenizer()
        for token in template_tokens(
                source_code, addition, token_class, split=False,
                macros=False):
            for tok in js_tokenizer(token):
                yield tok


# Tokens after which a JSX element can start an expression. After a name,
# `)` or `]`, an angle bracket is a comparison or type arguments instead.
# `}` and `>` keep sibling elements after a child expression or prior tag.
_BEFORE_EXPRESSION = {
    None, '(', '[', '{', '}', '>', ',', '=', '=>', 'return', '?', ':',
    '&&', '||', '??', '!', 'yield', 'default', 'case', ';'}


def _is_opening_tag(token):
    """`<p>` or `<Foo.Bar>`, which the tokenizer reads as one token."""
    return token.startswith('<') and token.endswith('>') and (
        token == '<>' or (len(token) > 2 and token[1] not in '/='))


class TSXTokenizer(JSTokenizer):
    def __init__(self):
        super().__init__()
        self._previous = None

    def process_token(self, token):
        previous = self._previous
        if not token.isspace():
            self._previous = token
        if token == "<":
            if previous in _BEFORE_EXPRESSION:
                self.sub_tokenizer = XMLTagWithAttrTokenizer(True)
            else:
                # Comparison or type arguments: `i < n`, `f<T>()`. Starting
                # a tag and aborting would hand `}` to brace counting and
                # end the enclosing expression too early.
                yield token
            return
        if _is_opening_tag(token) and previous in _BEFORE_EXPRESSION:
            # The children of an element are text except for {...}.
            self.sub_tokenizer = XMLTagWithAttrTokenizer.children_of(token)
            return

        if token == "=>":
            # Special handling for arrow functions
            yield token
            return

        for tok in super().process_token(token):
            yield tok


class XMLTagWithAttrTokenizer(Tokenizer):
    def __init__(self, in_expression=False):
        super(XMLTagWithAttrTokenizer, self).__init__()
        self.tag = None
        self._attribute = None
        self.state = self._global_state
        self.cache = ['<']
        # Token emitted when an embedded {expression} ends, if any.
        self._expression_end = None
        # Tag text flushed before an attribute expression starts.
        self._pending_flush = None
        # Where an expression is expected, `<` can only open an element,
        # so its children are text even when they hold `=`, `;` or `)`.
        self._in_expression = in_expression

    @classmethod
    def children_of(cls, opening_tag):
        tokenizer = cls(True)
        tokenizer.cache = [opening_tag]
        tokenizer.state = tokenizer._body
        return tokenizer

    def __call__(self, token):
        if self._pending_flush:
            for tok in self._pending_flush:
                yield tok
            self._pending_flush = None
        if self.sub_tokenizer:
            for tok in self.sub_tokenizer(token):
                yield tok
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
                # The TSXTokenizer consumed the closing '}'. After an
                # attribute expression, ';' closes any expression-body
                # arrow opened inside it (onClick={() => handler()}); a
                # child expression gets its '}' back.
                if self._expression_end:
                    yield self._expression_end
                    self._expression_end = None
            return
        for tok in self.process_token(token):
            yield tok

    def process_token(self, token):
        if token.isspace() and '\n' in token:
            # Attribute expressions pass through before the cached tag
            # text, so newlines go out now to keep their lines in order.
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
        self._aborted = True
        self.stop()
        return self.cache

    def flush(self):
        tmp, self.cache = self.cache, []
        text = ''.join(tmp)
        if text.isspace():
            # Readers drop whitespace tokens other than a bare newline.
            return ['\n'] * text.count('\n')
        return [text]

    def _global_state(self, token):
        if token == '>' and self._in_expression:
            self.state = self._body  # fragment: <>...</>
            return None
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
        # <Select<number> value={v} /> is an element; a generic call such
        # as useState<Result<T>>(x) has no attribute after them.
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
        elif token == "{":
            self._attribute_expression()  # spread: <L {...props} />
        elif isidentifier(token):
            self._attribute = token
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
        elif token == '-':
            self.state = self._attribute_name_part  # data-x, aria-hidden
        elif self._in_expression and self._attribute != 'extends':
            # A valueless attribute (<A x={1} flag>); `<T extends U>` is
            # a TypeScript type parameter instead.
            return self._after_tag(token)
        else:
            return self.abort()

    def _attribute_name_part(self, token):
        if isidentifier(token):
            self.state = self._expecting_equal_sign
        else:
            return self.abort()

    def _expecting_value(self, token):
        if token[0] in "'\"":
            self.state = self._after_tag
        elif token == "{":
            self.state = self._after_tag
            self._attribute_expression()

    def _attribute_expression(self):
        # Emit the tag text read so far before the expression so the state
        # machine meets `<D f={` before the expression's tokens, and an
        # abort cannot hand back an opening '{' whose '}' the expression
        # tokenizer consumes.
        self._pending_flush = self.flush()
        self.sub_tokenizer = TSXTokenizer()
        self._expression_end = ';'

    def _body(self, token):
        # Abort if token can't be JSX body content — likely a type
        # annotation close: React.FC<Props> = (...) => {
        if token in ('=', '=>', ';', ')') and not self._in_expression:
            return self.abort()

        if token == "<" or _is_opening_tag(token):
            self.cache.pop()
            if token == "<":
                self.sub_tokenizer = XMLTagWithAttrTokenizer(True)
            else:
                self.sub_tokenizer = self.children_of(token)
            return self.flush()

        if token.startswith("</"):
            self.stop()
            return self.flush()

        if token == '{':
            # The braces of a child expression stay visible, so the
            # expression reads as code between its own `{` and `}`.
            self.sub_tokenizer = TSXTokenizer()
            self._expression_end = '}'
            self.cache.pop()
            return [tok for tok in self.flush() if tok] + [JSXChildBrace('{')]


def isidentifier(token):
    try:
        return token.isidentifier()
    except AttributeError:
        return token.encode(encoding='UTF-8')[0].isalpha()
