'''
Language parser for JSX
'''

from .javascript import JavaScriptReader, JSTokenizer, XMLTagWithAttrTokenizer, isidentifier
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression


class JSXReader(JavaScriptReader):
    # pylint: disable=R0903

    ext = ['jsx']
    language_names = ['jsx']

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
