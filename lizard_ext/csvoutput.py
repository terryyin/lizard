"""
This module extends the default output formatting to include CSV.

The output is intended to be the same in structure, with additional
tokens (to reduce the need to post-process), and a reduced verbosity
due to the nature of CSV outputs. The differences are:

 * No summary of the included files, only the output
   respective of each function.
 * No additional output for functions that break any CCN
   thresholds.
 * Each line has four additional, individual tokens:
     * File name
     * Function Name
     * Function line start
     * Function line end
"""


def _extension_columns(extensions):
    """Collect FUNCTION_INFO field names and captions, in extension order."""
    variables = []
    captions = []
    for extension in extensions:
        if not hasattr(extension, "FUNCTION_INFO"):
            continue
        for name, info in extension.FUNCTION_INFO.items():
            variables.append(name)
            captions.append(info.get("caption", "No_caption"))
    return variables, captions


def csv_output(result, options):
    result = result.result
    extension_variables, extension_captions = _extension_columns(
        options.extensions)

    if options.verbose:
        print(",".join([
            "NLOC", "CCN", "token", "PARAM", "length", "location", "file",
            "function", "long_name", "start", "end",
        ] + extension_captions))

    for source_file in result:
        if not source_file:
            continue
        for source_function in source_file.function_list:
            if not source_function:
                continue
            name = source_function.name.replace('"', "'")
            long_name = source_function.long_name.replace('"', "'")
            fields = [
                source_function.nloc,
                source_function.cyclomatic_complexity,
                source_function.token_count,
                len(source_function.parameters),
                source_function.length,
                f'"{name}@{source_function.start_line}-'
                f'{source_function.end_line}@{source_file.filename}"',
                f'"{source_file.filename}"',
                f'"{name}"',
                f'"{long_name}"',
                source_function.start_line,
                source_function.end_line,
            ]
            fields.extend(
                getattr(source_function, variable)
                for variable in extension_variables)
            print(",".join(str(field) for field in fields))
