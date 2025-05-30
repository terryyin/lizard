'''
Language parser for TSX
'''

from .typescript import TypeScriptReader
from .jsx import JSXTypeScriptStates
from .code_reader import CodeReader
from .js_style_regex_expression import js_style_regex_expression


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
            r"|(?:\$\w+)" + \
            r"|(?:<\/\w+>)" + \
            r"|(?:=>)" + \
            r"|`.*?`"
        from .jsx import TSXTokenizer
        js_tokenizer = TSXTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok

    def __init__(self, context):
        super(TSXReader, self).__init__(context)
        # Use JSXTypeScriptStates for better handling of TSX specific features
        self.parallel_states = [JSXTypeScriptStates(context)]

    def _expecting_func_opening_bracket(self, token):
        # Handle TypeScript arrow functions with type annotations in JSX attributes
        if token == ':':
            self._consume_type_annotation()
            return
        if token == '<':
            self.next(self._expecting_jsx)
            return
        if token == '=>':
            self._handle_arrow_function()
            return
        super()._expecting_func_opening_bracket(token)

    def _handle_arrow_function(self):
        self.context.add_to_long_function_name(" => ")
        current_function = self.context.current_function
        self.context.restart_new_function('(anonymous)')
        def callback():
            self.context.current_function = current_function
        self.sub_state(self.__class__(self.context), callback)

    def _expecting_arrow_function_body(self, token):
        if token == '{':
            self.next(self._function_body)
        else:
            self.next(self._expecting_func_opening_bracket)

    def _function_body(self, token):
        if token == '}':
            self.context.end_of_function()
            self.next(self._expecting_func_opening_bracket)

    def _expecting_jsx(self, token):
        if token == '>':
            self.next(self._expecting_func_opening_bracket)
