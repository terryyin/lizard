'''
This is an extension of lizard, that counts the amount of dependencies
within the code.
'''


class LizardExtension(object):  # pylint: disable=R0903
    FUNCTION_CAPTION = " dep cnt "
    FUNCTION_INFO_PART = "dependency_count"

    def __call__(self, tokens, reader):
        ignored_list = {','}
        dependency_type = {
            'null': 0,
            '#include': 1,
            'import': 2,
            'python_import_as_change': 3}
        expect_dependency = 0
        import_list = []
        import_as_list = []
        import_as_counter = 0
        for token in tokens:
            if not hasattr(reader.context.current_function,
                           "dependency_count"):
                reader.context.current_function.dependency_count = 0
            # this accounts for java, c, c++ and python's import
            if token == "import" or token == "#include":
                if import_as_list != []:
                    import_list.append(import_as_list)
                expect_dependency = dependency_type[token]
            elif expect_dependency == dependency_type['#include']:
                # gets rid of the <> or "" as well as the .h
                import_list += [token[1:len(token) - 3]]
                expect_dependency = dependency_type['null']
            elif expect_dependency == dependency_type['import']:
                if token == "as":
                    expect_dependency = dependency_type[
                        'python_import_as_change']
                    import_as_counter = len(import_as_list)
                    import_as_list = []
                elif import_as_counter > 4:
                    import_list += [import_as_list[0]]
                    import_as_list = []
                    import_as_counter = 0
                    expect_dependency = dependency_type['null']
                elif token not in ignored_list:
                    import_as_counter += 1
                    import_as_list += [token]
            elif (expect_dependency ==
                  dependency_type['python_import_as_change'] and
                  token not in ignored_list):
                import_as_counter -= 1
                import_list += [token]
                if import_as_counter == 0:
                    expect_dependency = dependency_type['null']
            if token in import_list:
                reader.context.current_function.dependency_count += 1
            yield token
