'''
Language parser for Vue.js files
'''

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .javascript import JavaScriptReader
from .typescript import TypeScriptReader


class VueReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue', 'vuejs']

    def __init__(self, context):
        super(VueReader, self).__init__(context)
        self._sub_reader = None
        self.parallel_states = []  # We'll create states when we know the script type

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Use the base token generator but ensure we capture Vue block tags
        addition = addition + r"|<\/?script.*?>"
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        current_block = None
        script_lang = None
        script_content = []
        
        for token in tokens:
            if token.startswith('<script'):
                current_block = 'script'
                if 'lang="ts"' in token:
                    script_lang = 'ts'
                    self._sub_reader = TypeScriptReader(self.context)
                else:
                    script_lang = 'js'
                    self._sub_reader = JavaScriptReader(self.context)
                
                if self._sub_reader:
                    self.parallel_states = self._sub_reader.parallel_states
                continue
            elif token.startswith('</script'):
                # Process accumulated script content
                if script_content and self._sub_reader:
                    script_text = ''.join(script_content)
                    for processed_token in self._sub_reader.generate_tokens(script_text):
                        if not processed_token.isspace():
                            yield processed_token
                current_block = None
                script_content = []
                self._sub_reader = None
                self.parallel_states = []
                continue
            elif token.startswith('<template') or token.startswith('<style'):
                current_block = 'ignore'
                continue
            elif token.startswith('</template') or token.startswith('</style'):
                current_block = None
                continue

            if current_block == 'script':
                script_content.append(token)


class VueStates(CodeStateMachine):
    def _state_global(self, token):
        pass  # Vue state machine delegates actual processing to sub-readers 