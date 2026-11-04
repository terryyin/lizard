from lizard import analyze_file


def analyze_fsharp(source_code):
    return analyze_file.analyze_source_code("a.fs", source_code)


def get_fsharp_function_list(source_code):
    return analyze_fsharp(source_code).function_list


def spans(source_code):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in get_fsharp_function_list(source_code)]


def by_name(source_code):
    return {f.name: f for f in get_fsharp_function_list(source_code)}
