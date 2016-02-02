'''
Common behaviours of script languages
'''


class ScriptLanguageMixIn(object):
    # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("#"):
            return token[1:]
