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
        raw_openning = r"\<(\w+)\s*(\/?)\>"
        raw_closing = r"\<\/(\w+)\>"
        addition = addition +\
                      r"|(?:\$\w+)" + \
                      r"|(?:"+raw_openning + r")" + \
                      r"|(?:"+raw_closing + r")" + \
                      r"|(?:\<\/\w+\>)" + \
                      r"|`.*?`"

        xml_openning = re.compile(raw_openning)
        xml_closing = re.compile(raw_closing)

        class JSTokenizer:
            def __init__(self):
                self.xml = None
                self.openning = None

            def __call__(self, token):
                if self.xml:
                    for tk in self.xml(token):
                        yield tk
                    if self.xml.ended:
                        self.xml = None
                    return
                mch = xml_openning.match(token)
                if not self.openning and mch:
                    if mch[2] != '/':
                        self.openning = mch[1]
                        self.xml = XMLTokenizer(mch[1])
                        return
                yield token

        class XMLTokenizer:
            def __init__(self, tag):
                self.tag = tag
                self.depth = 1
                self.saved = '<'+tag+'>'
                self.ended = False

            def __call__(self, token):
                if token == "}":
                    self.saved = token
                    return
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
                    self.saved += token
                    if token == '{':
                        yield self.saved
                        self.saved = ""

        js_tokenizer = JSTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tk in js_tokenizer(token):
                yield tk

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
        self.parallel_states = [JavaScriptStyleLanguageStates(context)]
