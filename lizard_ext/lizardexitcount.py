"""
This is an extension of lizard, that counts the 'exit points'
in every function.
"""


class LizardExtension:  # pylint: disable=R0903

    FUNCTION_INFO = {"exit_count": {"caption": "exits", "default": 1}}

    def __call__(self, tokens, reader):
        for token in tokens:
            if token == 'return' and not getattr(token, 'is_member_name', False):
                function = reader.context.current_function
                function.exit_count = getattr(function, 'exit_count', 0) + 1
            yield token
