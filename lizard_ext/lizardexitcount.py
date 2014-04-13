'''
This is an extension of lizard, that counts the 'exit points'
in every function.
'''


class LizardExtension(object):  # pylint: disable=R0903

    FUNCTION_CAPTION = " exits "
    FUNCTION_INFO_PART = "exit_count"

    def __call__(self, tokens, reader):
        first_return = False
        for token in tokens:
            if not hasattr(reader.context.current_function, "exit_count"):
                reader.context.current_function.exit_count = 1
                first_return = True
            if token == "return":
                if first_return:
                    first_return = False
                else:
                    reader.context.current_function.exit_count += 1
            yield token
