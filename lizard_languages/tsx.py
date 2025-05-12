'''
Language parser for TSX
'''

from .typescript import TypeScriptReader
from .jsx import JSXMixin, JSXTypeScriptStates
from .js_style_regex_expression import js_style_regex_expression


class TSXReader(TypeScriptReader, JSXMixin):
    # pylint: disable=R0903

    ext = ['tsx']
    language_names = ['tsx']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Add support for TypeScript type annotations in JSX
        addition = addition + \
            r"|(?:<[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)" + \
            r"|(?:<\/[A-Za-z][A-Za-z0-9]*(?:\.[A-Za-z][A-Za-z0-9]*)*>)"
        return JSXMixin.generate_tokens(source_code, addition, token_class)

    def __init__(self, context):
        super(TSXReader, self).__init__(context)
        # Use JSXTypeScriptStates for better handling of TSX specific features
        self.parallel_states = [JSXTypeScriptStates(context)]

    def _expecting_func_opening_bracket(self, token):
        # Handle TypeScript arrow functions with type annotations in JSX attributes
        if token == ':':
            self._consume_type_annotation()
            return
        super()._expecting_func_opening_bracket(token)
