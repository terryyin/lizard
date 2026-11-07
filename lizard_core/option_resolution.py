"""
Resolving the parsed command-line options into extensions, thresholds and the
output printer.
"""
import sys
from lizard_ext import print_xml, print_csv, html_output, print_checkstyle
from lizard_core.extensions import get_extensions
from lizard_core.output_scheme import OutputScheme
from lizard_core.options import _extension_arg, arg_parser


def parse_args(argv):
    def extend_parser(parser_to_extend):
        from argparse import ArgumentParser
        parser = ArgumentParser(add_help=False)
        _extension_arg(parser)
        opt, _ = parser.parse_known_args(args=argv[1:])
        extensions = get_extensions(opt.extensions)
        for ext in extensions:
            if hasattr(ext, "set_args"):
                ext.set_args(parser_to_extend)  # pylint: disable=E1101
        return parser_to_extend
    parser = extend_parser(arg_parser(argv[0]))
    opt = parser.parse_args(args=argv[1:])
    opt.extensions = get_extensions(opt.extensions)
    values = OutputScheme(opt.extensions).value_columns()
    no_fields = (set(opt.sorting) | set(opt.thresholds.keys())) - set(values)
    if no_fields:
        error_message = "Wrong field name '%s'.\n" % ", ".join(no_fields)
        error_message += "Candidates are: " + ', '.join(values) + "\n"
        sys.stderr.write(error_message)
        sys.exit(2)
    if "cyclomatic_complexity" not in opt.thresholds:
        opt.thresholds["cyclomatic_complexity"] = opt.CCN
    if "max_nesting_depth" not in opt.thresholds and hasattr(opt, "ND"):
        opt.thresholds["max_nesting_depth"] = opt.ND
    if "max_nested_structures" not in opt.thresholds and hasattr(opt, "NS"):
        opt.thresholds["max_nested_structures"] = opt.NS
    if "cognitive_complexity" not in opt.thresholds and hasattr(opt, "CogC"):
        opt.thresholds["cognitive_complexity"] = opt.CogC
    if "length" not in opt.thresholds:
        opt.thresholds["length"] = opt.length
    if "nloc" not in opt.thresholds:
        opt.thresholds["nloc"] = 1000000
    if "parameter_count" not in opt.thresholds:
        opt.thresholds["parameter_count"] = opt.arguments
    if opt.output_file:
        inferred_printer = infer_printer_from_file_ext(opt.output_file)
        if inferred_printer:
            # Always use print_checkstyle for .checkstyle.xml
            if opt.output_file.lower().endswith('.checkstyle.xml'):
                opt.printer = inferred_printer
            elif not opt.printer:
                opt.printer = inferred_printer
            elif opt.printer != inferred_printer:
                msg = "Warning: overriding output file extension.\n"
                sys.stderr.write(msg)
    return opt


def infer_printer_from_file_ext(path):
    lower_path = path.lower()
    if lower_path.endswith(".checkstyle.xml"):
        return print_checkstyle
    if lower_path.endswith(".html"):
        return html_output
    if lower_path.endswith(".xml"):
        return print_xml
    if lower_path.endswith(".csv"):
        return print_csv
    return None
