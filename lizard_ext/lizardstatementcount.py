"""
This is an extension of lizard, that counts the statements in a function
"""


class LizardExtension:  # pylint: disable=R0903

    FUNCTION_INFO = {"statement_count": {"caption": "statements"}}

    def __call__(self, tokens, reader):
        for token in tokens:
            if not hasattr(reader.context.current_function, "statement_count"):
                reader.context.current_function.statement_count = 0
            if token == ";":
                reader.context.current_function.statement_count += 1
            if token == "if":
                reader.context.current_function.statement_count += 1
            if token == "for":
                reader.context.current_function.statement_count += 1
            if token == "while":
                reader.context.current_function.statement_count += 1
            yield token
