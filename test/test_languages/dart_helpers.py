from lizard import analyze_file


def get_dart_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.dart", source_code
    ).function_list
