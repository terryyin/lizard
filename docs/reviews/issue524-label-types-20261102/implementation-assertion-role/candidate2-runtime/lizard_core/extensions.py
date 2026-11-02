"""
Extension loading: the default token processors plus the extensions named
with -E, loaded by module name from lizard_ext, and their final results.
"""
from lizard_core.file_analyzer import preprocessing, comment_counter, \
    line_counter, token_counter, condition_counter


def get_extensions(extension_names):
    from importlib import import_module as im

    def expand_extensions(existing):
        for name in extension_names:
            ext = (
                    im('lizard_ext.lizard' + name.lower())
                    .LizardExtension()
                    if isinstance(name, str) else name)
            existing.insert(
                len(existing) if not hasattr(ext, "ordering_index") else
                ext.ordering_index,
                ext)
        return existing

    return expand_extensions([
            preprocessing,
            comment_counter,
            line_counter,
            token_counter,
            condition_counter,
        ])


def print_extension_results(extensions):
    for extension in extensions:
        if hasattr(extension, 'print_result'):
            extension.print_result()
