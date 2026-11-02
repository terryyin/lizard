'''
This is an extension of lizard, that counts the amount of goto's
'''


class LizardExtension():  # pylint: disable=R0903

    FUNCTION_INFO = {"goto_count": {"caption": " goto's "}}

    def __call__(self, tokens, reader):
        for token in tokens:
            if not hasattr(reader.context.current_function, "goto_count"):
                reader.context.current_function.goto_count = 0
            if token == "goto":
                reader.context.current_function.goto_count += 1
            yield token
