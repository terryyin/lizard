from lizard import analyze_file


def get_swift_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.swift", source_code).function_list
