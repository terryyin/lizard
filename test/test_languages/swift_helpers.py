from lizard import analyze_file


def get_swift_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.swift", source_code).function_list


def swift_function_spans(source_code):
    return [(function.name, function.start_line, function.end_line,
             function.cyclomatic_complexity)
            for function in get_swift_function_list(source_code)]
