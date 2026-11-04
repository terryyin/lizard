"""
Warning policy: selecting the functions that exceed the thresholds, minus
those forgiven or listed in the whitelist.
"""
import os
from lizard_ext import auto_read

DEFAULT_WHITELIST = "whitelizard.txt"


def exceeds_thresholds(option, fun):
    violated_metrics = [
        attr for attr, limit in option.thresholds.items()
        if getattr(fun, attr) > limit
    ]
    forgiven = getattr(fun, 'forgiven_metrics', set())
    return any(metric not in forgiven for metric in violated_metrics)


def _functions_matching(module_infos, predicate):
    return (fun for file_info in module_infos if file_info
            for fun in file_info.function_list if predicate(fun))


def warning_filter(option, module_infos):
    return _functions_matching(
        module_infos, lambda fun: exceeds_thresholds(option, fun))


def whitelist_matcher(script=None, whitelist=None):
    def _get_whitelist_item(script):
        white = {}
        pieces = script.replace('::', '##').split(':')
        if len(pieces) > 1:
            white['file_name'] = pieces[0]
            script = pieces[1]
        white['function_names'] = (
            [x.strip().replace('##', '::') for x in script.split(',')])
        return white

    def _in_list(warning):
        return any(_match_whitelist_item(white, warning)
                   for white in whitelist)

    def _match_whitelist_item(white, warning):
        return (warning.name in white['function_names'] and
                warning.filename == white.get('file_name', warning.filename))

    def get_whitelist(whitelist):
        if os.path.isfile(whitelist):
            return auto_read(whitelist)
        if whitelist != DEFAULT_WHITELIST:
            print("WARNING: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            print("WARNING: the whitelist \""+whitelist+"\" doesn't exist.")
        return ''

    if not script:
        script = get_whitelist(whitelist)
    whitelist = [
        _get_whitelist_item(line.split('#')[0])
        for line in script.splitlines()]
    return _in_list


def whitelist_filter(warnings, script=None, whitelist=None):
    whitelisted = whitelist_matcher(script, whitelist)
    for warning in warnings:
        if not whitelisted(warning):
            yield warning


def warning_policy(option):
    whitelisted = whitelist_matcher(whitelist=option.whitelist)
    return lambda fun: exceeds_thresholds(option, fun) and not whitelisted(fun)


def get_warnings(code_infos, option, is_warning=None):
    is_warning = is_warning or warning_policy(option)
    warnings = _functions_matching(code_infos, is_warning)
    if isinstance(option.sorting, list) and option.sorting:
        warnings = sorted(warnings, reverse=True, key=lambda x: getattr(
            x, option.sorting[0]))
    return warnings
