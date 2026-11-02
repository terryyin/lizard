"""
The command entrance: parse the options, analyze the paths, print the result
to stdout or the output file, and turn the warning count into an exit code.
"""
import codecs
import sys
from lizard_ext import auto_read
from lizard_core.function_info import AllResult
from lizard_core.extensions import print_extension_results
from lizard_core.analysis import analyze
from lizard_core.text_output import print_result, silent_printer
from lizard_core.options import parse_args
from lizard_core.output_scheme import OutputScheme


def open_output_file(path):
    try:
        return codecs.open(path, 'w', encoding='utf8')
    except OSError:
        msg = "Error: failed to open output file '{}'\n.".format(path)
        sys.stderr.write(msg)
        sys.exit(2)


def main(argv=None):
    """Command-line entrance to Lizard.

    Args:
        argv: Arguments vector; if None, sys.argv by default.
    """
    options = parse_args(argv or sys.argv)
    printer = options.printer or print_result
    schema = OutputScheme(options.extensions)
    if schema.any_silent():
        printer = silent_printer
    schema.patch_for_extensions()
    if options.input_file:
        options.paths = auto_read(options.input_file).splitlines()
    original_stdout = sys.stdout
    output_file = None
    result = analyze(
        options.paths,
        options.exclude,
        options.working_threads,
        options.extensions,
        options.languages,
        options.use_gitignore)
    warning_count = None
    if options.output_file:
        output_file = open_output_file(options.output_file)
        sys.stdout = output_file
        # Special handling for checkstyle output
        if getattr(printer, "__name__", "") == "print_checkstyle":
            warning_count = printer(result, options, schema, AllResult, file=output_file)
        else:
            warning_count = printer(result, options, schema, AllResult)
    else:
        # Special handling for checkstyle output
        if getattr(printer, "__name__", "") == "print_checkstyle":
            warning_count = printer(result, options, schema, AllResult, file=original_stdout)
        else:
            warning_count = printer(result, options, schema, AllResult)
    print_extension_results(options.extensions)
    list(result)
    if output_file:
        sys.stdout = original_stdout
        output_file.close()
    if 0 <= options.number < warning_count:
        return 1
    return 0
