'''
This is an extension of lizard, that counts the amount of dependencies
within the code.
'''


class LizardExtension(object):  # pylint: disable=R0903
    FUNCTION_CAPTION = " dep cnt "
    FUNCTION_INFO_PART = "dependency_count"

    def __call__(self, tokens, reader):
        dependency_type = {'import': 1, '#include': 2}
        next_word_is_a_dependency = 0
        import_list = []
        for token in tokens:
            if not hasattr(reader.context.current_function,
                           "dependency_count"):
                reader.context.current_function.dependency_count = 0
            # this accounts for java, c, c++ and python's import
            if token == "import" or token == "#include":
                next_word_is_a_dependency = dependency_type[token]
            elif next_word_is_a_dependency == dependency_type['import']:
                import_list += [token]
                next_word_is_a_dependency = 0
            elif next_word_is_a_dependency == dependency_type['#include']:
                #gets rid of the <> or "" as well as the .h
                if len(token) >= 4:
                    import_list += [token[1:len(token) - 3]]
                else:
                    import_list += [token[1:len(token) - 1]]
                next_word_is_a_dependency = 0

            if token in import_list:
                reader.context.current_function.dependency_count += 1
            yield token
