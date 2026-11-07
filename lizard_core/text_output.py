"""
The default text printers and the clang/MSVS style warning printers.
"""
from lizard_core.warning_filter import get_warnings, warning_policy


def print_warnings(option, scheme, warnings):
    warning_count = 0
    warning_nloc = 0
    warn_str = "!!!! Warnings ({0}) !!!!".format(
        ' or '.join("{0} > {1}".format(
            k, val) for k, val in option.thresholds.items()))
    for warning in warnings:
        if warning_count == 0:
            print("\n" + "=" * len(warn_str) + "\n" + warn_str)
            print(scheme.function_info_head())
        warning_count += 1
        warning_nloc += warning.nloc
        print(scheme.function_info(warning))
    if warning_count == 0:
        print_no_warnings(option)
    return warning_count, warning_nloc


def print_no_warnings(option):
    warn_str = "No thresholds exceeded ({0})".format(
        ' or '.join("{0} > {1}".format(
            k, val) for k, val in option.thresholds.items()))
    print("\n" + "=" * len(warn_str) + "\n" + warn_str)


def print_total(warning_count, warning_nloc, all_result, scheme):
    print("=" * 90)
    print("Total nloc  " + scheme.average_captions() + "  Fun Cnt  Warning"
          " cnt   Fun Rt   nloc Rt")
    print("-" * 90)
    print((
        "{module.nloc:10d}" +
        scheme.average_formatter() +
        "{function_count:9d}{warning_count:13d}" +
        "{function_rate:10.2f}{nloc_rate:8.2f}").format(
                  module=all_result.as_fileinfo(),
                  function_count=all_result.function_count(),
                  warning_count=warning_count,
                  function_rate=(warning_count/all_result.function_count()),
                  nloc_rate=(warning_nloc/all_result.nloc_in_functions())))


def print_and_save_modules(all_fileinfos, scheme, shown=lambda fun: True):
    saved_fileinfos = []
    print(scheme.function_info_head())
    for module_info in all_fileinfos:
        if module_info:
            saved_fileinfos.append(module_info)
            for fun in filter(shown, module_info.function_list):
                try:
                    print(scheme.function_info(fun))
                except UnicodeEncodeError:
                    print("Found ill-formatted unicode function name.")
    print("%d file analyzed." % (len(saved_fileinfos)))
    print("==============================================================")
    print("NLOC   " + scheme.average_captions() + " function_cnt    file")
    print("--------------------------------------------------------------")
    for module_info in saved_fileinfos:
        print((
            "{module.nloc:7d}" +
            scheme.average_formatter() +
            "{function_count:10d}" +
            "     {module.filename}").format(
            module=module_info,
            function_count=len(module_info.function_list)))
    return saved_fileinfos


def is_trivial_nested_callback(fun):
    return (fun.name == '(anonymous)' and fun.defined_in_function and
            fun.cyclomatic_complexity == 1)


def print_result(result, option, scheme, total_factory):
    # Only --compact needs the warning policy before the table; building it
    # later keeps the default report's missing-whitelist notice after it.
    is_warning = warning_policy(option) if option.compact else None

    def shown(fun):
        return (is_warning is None or is_warning(fun) or
                not is_trivial_nested_callback(fun))
    result = print_and_save_modules(result, scheme, shown)
    warnings = get_warnings(result, option, is_warning)
    warning_count, warning_nloc = print_warnings(option, scheme, warnings)
    print_total(warning_count, warning_nloc, total_factory(result), scheme)
    return warning_count


def silent_printer(result, *_):
    '''
    just to exhaust the result, no output.
    '''
    for _ in result:
        pass
    return 0


def print_clang_style_warning(code_infos, option, scheme, _):
    count = 0
    for warning in get_warnings(code_infos, option):
        print(scheme.clang_warning_format().format(f=warning))
        count += 1
    return count


def print_msvs_style_warning(code_infos, option, scheme, _):
    count = 0
    for warning in get_warnings(code_infos, option):
        print(scheme.msvs_warning_format().format(f=warning))
        count += 1
    return count
