'''
Common behaviours of script languages
'''
from .code_reader import CodeReader


class ScriptLanguageMixIn:
    # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("#"):
            # For forgiveness comments, return the entire comment with directive intact
            stripped = token.lstrip('#').strip()

            # Handle forgiveness directives with proper formatting
            if stripped.startswith('lizard forgive global') or stripped.startswith('#lizard forgive global'):
                return '#lizard forgive global'  # Preserve global directive
            elif stripped.startswith('lizard forgive') or stripped.startswith('#lizard forgive'):
                return '#lizard forgive'  # Return standardized forgiveness comment

            return stripped  # Return the stripped comment for other cases
        return None

    @staticmethod
    def generate_common_tokens(source_code, addition, match_holder=None):
        _until_end = r"(?:\\\n|[^\n])*"
        return CodeReader.generate_tokens(
            source_code,
            r"|\#" + _until_end + addition,
            match_holder)
