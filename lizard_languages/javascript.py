'''
Language parser for JavaScript
'''

from .typescript import TypeScriptReader


class JavaScriptReader(TypeScriptReader):
    # pylint: disable=R0903

    ext = ['js', 'cjs', 'mjs']
    language_names = ['javascript', 'js']

    def __init__(self, context):
        super(JavaScriptReader, self).__init__(context)
