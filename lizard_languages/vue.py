'''
Language parser for Vue.js
'''

from .javascript import JavaScriptReader, JSTokenizer
from .typescript import TypeScriptReader, TypeScriptObjectStates
from .code_reader import CodeReader
from .js_style_language_states import JavaScriptStyleLanguageStates, ES6ObjectStates
from .typescript import TypeScriptStates
from .clike import CCppCommentsMixin
import re


class VueObjectStates(TypeScriptObjectStates):
    def _state_global(self, token):
        if token == '(' and self.last_tokens:
            # Handle method without type annotation
            self._function(self.last_tokens)
            self.next(self._function, token)
            return
        super(VueObjectStates, self)._state_global(token)


class VueReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue']

    def __init__(self, context):
        super(VueReader, self).__init__(context)
        self._context = context
        self._state = None
        self._setup_state_based_on_content = True

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def _extract_script_content(self, source):
        script_pattern = re.compile(r'<script.*?>(.*?)</script>', re.DOTALL)
        match = script_pattern.search(source)
        if not match:
            return ''
        return match.group(1).strip()

    def _is_typescript(self, script_content):
        lang_pattern = re.compile(r'<script.*?lang=[\'"](ts|typescript)[\'"].*?>', re.DOTALL)
        return bool(lang_pattern.search(script_content))

    def _setup_state(self, tokens):
        if not self._setup_state_based_on_content:
            return
        
        content = ''.join(tokens)
        script_content = self._extract_script_content(content)
        
        if not script_content:
            self.parallel_states = []
            return

        state = VueObjectStates(self._context)
        state.read_object = lambda: state.sub_state(VueObjectStates(self._context))
        self.parallel_states = [state]

        if self._is_typescript(content):
            tokens = TypeScriptReader.generate_tokens(script_content)
        else:
            js_tokenizer = JSTokenizer()
            tokens = (tok for token in JavaScriptReader.generate_tokens(script_content) 
                     for tok in js_tokenizer(token))
        
        self._setup_state_based_on_content = False
        return tokens

    def preprocess(self, tokens):
        tokens = list(tokens)
        processed_tokens = self._setup_state(tokens)
        if processed_tokens:
            return list(processed_tokens)
        return [] 