'''
Language parser for JavaScript
'''

from .js_style_regex_expression import js_style_regex_expression
from .typescript import TypeScriptReader


class JavaScriptReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['js', 'cjs', 'mjs']
    language_names = ['javascript', 'js']
    _class_field_annotations = False

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)

    generate_tokens = staticmethod(js_style_regex_expression(
        TypeScriptReader.generate_tokens.__wrapped__, language='javascript'))
