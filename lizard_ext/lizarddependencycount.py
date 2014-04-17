'''
This is an extension of lizard, that counts the amount of dependencies
within the code.
'''


class LizardExtension(object):
    FUNCTION_CAPTION = " dep cnt "
    FUNCTION_INFO_PART = "dependency_count"

    def __call__(self, tokens, reader):
        next_word_is_a_dependency = False
        import_list = []
        for token in tokens:
            if not hasattr(reader.context.current_function,
                           "dependency_count"):
                reader.context.current_function.dependency_count = 0
            # this accounts for java, c, c++ and python's import
            if token == "import" or token == "#include":
                next_word_is_a_dependency = True
            elif next_word_is_a_dependency:
                import_list += [token]
                next_word_is_a_dependency = False

            if token in import_list:
                reader.context.current_function.dependency_count += 1
            yield token
