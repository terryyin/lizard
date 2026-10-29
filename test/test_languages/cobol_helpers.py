from lizard import analyze_file


def get_cobol_function_list(source_code):
    return analyze_file.analyze_source_code("a.cbl", source_code).function_list


def fixed(lines):
    """Build fixed-format COBOL: 6-digit seq + indicator + body."""
    out = []
    for i, line in enumerate(lines, start=1):
        if isinstance(line, tuple):
            indicator, body = line
        else:
            indicator, body = ' ', line
        out.append('%06d%s%s\n' % (i * 100, indicator, body))
    return ''.join(out)
