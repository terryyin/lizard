'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates


class JavaScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['js']
    language_names = ['javascript', 'js']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        raw_openning = r"\<(\w+)\s*(?P<self_close>\/?)\>"
        raw_closing = r"\<\/(\w+)\>"
        addition = addition +\
            r"|(?:\$\w+)" + \
            r"|(?:"+raw_openning + r")" + \
            r"|(?:"+raw_closing + r")" + \
            r"|(?:\<\/\w+\>)" + \
            r"|`.*?`"

        xml_openning = re.compile(raw_openning)
        xml_closing = re.compile(raw_closing)

        class Tokenizer:
            def __init__(self):
                self.sub_tokenizer = None
                self.ended = False

            def __call__(self, token):
                if self.sub_tokenizer:
                    for tk in self.sub_tokenizer(token):
                        yield tk
                    if self.sub_tokenizer.ended:
                        self.on_sub_ended()
                        self.sub_tokenizer = None
                    return
                for tk in self.process_token(token):
                    yield tk

            def on_sub_ended(self):
                pass

        class JSTokenizer(Tokenizer):
            def __init__(self):
                super(JSTokenizer, self).__init__()
                self.openning = None

            def process_token(self, token):
                if token == "<":
                    self.sub_tokenizer = XMLTagWithAttrTokenizer()
                    return
                if token == "{":
                    self.sub_tokenizer = JSTokenizer()
                elif token == "}":
                    self.ended = True
                else:
                    mch = xml_openning.match(token)
                    if not self.openning and mch:
                        if mch['self_close'] != '/':
                            self.openning = mch[1]
                            self.sub_tokenizer = XMLTokenizer(mch[1])
                            return
                yield token

        class XMLTagWithAttrTokenizer(Tokenizer):
            def __init__(self):
                super(XMLTagWithAttrTokenizer, self).__init__()
                self.tag = None
                self.state = self._global_state
                self.cache = ['<']

            def process_token(self, token):
                self.cache.append(token)
                if not token.isspace():
                    return self.state(token)
                return ()

            def abort(self):
                self.ended = True
                return self.cache

            def _global_state(self, token):
                if not token.isidentifier():
                    return self.abort()
                self.tag = token
                self.state = self._after_tag
                return ()

            def _after_tag(self, token):
                if token == '>':
                    self.sub_tokenizer = XMLTokenizer(self.tag)
                    self.state = None
                elif token.isidentifier():
                    self.state = self._expecting_equal_sign
                else:
                    return self.abort()
                return ()

            def _expecting_equal_sign(self, token):
                if token == '=':
                    self.state = self._expecting_value
                else:
                    return self.abort()
                return ()

            def _expecting_value(self, token):
                if token[0] in "'\"":
                    self.state = self._after_tag
                elif token == "{":
                    yield token
                    self.sub_tokenizer = JSTokenizer()
                    self.state = self._after_tag
                return ()

            def on_sub_ended(self):
                if self.state is None:
                    self.ended = True

        class XMLTokenizer(Tokenizer):
            def __init__(self, tag):
                super(XMLTokenizer, self).__init__()
                self.tag = tag
                self.depth = 1
                self.saved = '<'+tag+'>'

            def process_token(self, token):

                mch = xml_openning.match(token)
                if mch and mch[1] == self.tag:
                    self.depth += 1
                mch = xml_closing.match(token)
                if mch and mch[1] == self.tag:
                    self.depth -= 1
                    if self.depth == 0:
                        yield self.saved + token
                        self.saved = ''
                        self.ended = True
                        return
                if self.saved:
                    if token == '{':
                        yield self.saved
                        self.saved = ""
                        yield '{'
                        self.sub_tokenizer = JSTokenizer()
                    else:
                        self.saved += token

        js_tokenizer = JSTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tk in js_tokenizer(token):
                yield tk

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
        self.parallel_states = [JavaScriptStyleLanguageStates(context)]
