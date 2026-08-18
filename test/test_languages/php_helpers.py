from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_php_function_list(source_code):
    return analyze_file.analyze_source_code("a.php", source_code).function_list


def get_php_function_list_with_nesting_depth(source_code):
    return FileAnalyzer(get_extensions([NestDepth()])).analyze_source_code(
        "a.php", source_code).function_list
