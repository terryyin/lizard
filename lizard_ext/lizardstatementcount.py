"""
This is an extension of lizard, that counts the statements in a function
"""


class LizardExtension:  # pylint: disable=R0903

    FUNCTION_INFO = {"statement_count": {"caption": "statements"}}

    def __call__(self, tokens, reader):
        c_family = 'c' in reader.language_names or \
                   'cpp' in reader.language_names

        block_count = 0
        for token in tokens:
            if c_family:
                if not hasattr(reader.context.current_function,
                               "statement_count"):
                    reader.context.current_function.statement_count = 0
                if token in [';', 'if', 'for', 'while', ':', 'switch']:
                    reader.context.current_function.statement_count += 1
                if token == "{":
                    if block_count != 0:
                        reader.context.\
                            current_function.statement_count += 1
                    block_count += 1
                if token == "}":
                    block_count -= 1 if block_count > 0 else 0
            else:
                reader.context.current_function.statement_count = None

            yield token
