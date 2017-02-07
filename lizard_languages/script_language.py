'''
Common behaviours of script languages
'''
from .code_reader import CodeReader


class ScriptLanguageMixIn(object):
    # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("#"):
            return token[1:]

    @staticmethod
    def generate_common_tokens(source_code, addition, match_holder=None):
        _until_end = r"(?:\\\n|[^\n])*"
        return CodeReader.generate_tokens(
            source_code,
            r"|\#" + _until_end + addition,
            match_holder)
