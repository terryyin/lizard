from lizard import analyze_file


def get_ocaml_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.ml", source_code
    ).function_list


def spans(source_code):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in get_ocaml_function_list(source_code)]
