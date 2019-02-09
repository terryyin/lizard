'''
Language parser for Lua
'''

from .rubylike import RubylikeReader


class LuaReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['lua']
    language_names = ['lua']

    FUNCTION_DECLARE = 'function'

    def __init__(self, context):
        super(LuaReader, self).__init__(context, FUNCTION='function')

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return RubylikeReader.generate_tokens(
            source_code,
            r"|\-\-\[\[.*?\]\]" +
            r"|\[\=*\[.*?\]\=*\]" +
            r"|\-\-.*?$" +
            addition)

    def get_comment_from_token(self, token):
        if token.startswith("--"):
            return token
