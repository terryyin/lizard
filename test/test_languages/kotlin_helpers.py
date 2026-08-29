from lizard import analyze_file


def get_kotlin_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.kt", source_code
    ).function_list
