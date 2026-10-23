"""
Warning policy: selecting the functions that exceed the thresholds, minus
those forgiven or listed in the whitelist.
"""
import os
from lizard_ext import auto_read

DEFAULT_WHITELIST = "whitelizard.txt"


def warning_filter(option, module_infos):
    for file_info in module_infos:
        if file_info:
            for fun in file_info.function_list:
                violated_metrics = [
                    attr for attr, limit in option.thresholds.items()
                    if getattr(fun, attr) > limit
                ]
                if not violated_metrics:
                    continue
                forgiven = getattr(fun, 'forgiven_metrics', set())
                if all(metric in forgiven for metric in violated_metrics):
                    continue
                yield fun


def whitelist_filter(warnings, script=None, whitelist=None):
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
    for warning in warnings:
        if not _in_list(warning):
            yield warning


def get_warnings(code_infos, option):
    warnings = whitelist_filter(warning_filter(option, code_infos),
                                whitelist=option.whitelist)
    if isinstance(option.sorting, list) and option.sorting:
        warnings = sorted(warnings, reverse=True, key=lambda x: getattr(
            x, option.sorting[0]))
    return warnings
