#!/usr/bin/env python
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  author: terry@odd-e.com
#  Website: www.lizard.ws
#
"""
lizard is an extensible Cyclomatic Complexity Analyzer for many programming
languages including C/C++ (doesn't require all the header files).
For more information visit http://www.lizard.ws
"""
# The public facade: every name below is re-exported for users and
# extensions; the definitions live in lizard_core.
# pylint: disable=unused-import
import sys
import os

# Add script directory to sys.path for Python embeddable package / running from source
# See: https://github.com/terryyin/lizard/issues/460
# Use both __file__ and sys.argv[0]: embeddable Python on Windows may resolve them differently


def _script_dirs():
    dirs = []
    try:
        d = os.path.dirname(os.path.abspath(__file__))
        if d:
            dirs.append(d)
    except NameError:
        pass
    if sys.argv and sys.argv[0]:
        d = os.path.dirname(os.path.abspath(sys.argv[0]))
        if d and d not in dirs:
            dirs.append(d)
    return dirs


for _script_dir in _script_dirs():
    if _script_dir not in sys.path:
        sys.path.insert(0, _script_dir)


try:
    from lizard_languages import languages, get_reader_for, CLikeReader
except ImportError:
    sys.stderr.write("Cannot find the lizard_languages module.")
    sys.exit(2)
try:
    from lizard_ext import version
    from lizard_ext import print_xml
    from lizard_ext import print_csv
    from lizard_ext import html_output
    from lizard_ext import auto_open, auto_read
    from lizard_ext import print_checkstyle
except ImportError:
    sys.stderr.write("Cannot find the lizard_ext modules.")
from lizard_core.file_discovery import (  # noqa: E402
    md5_hash_file, _build_gitignore_spec, get_all_source_files)
from lizard_core.function_info import (  # noqa: E402
    Nesting, BARE_NESTING, Namespace, FunctionInfo, FileInformation,
    AllResult)
from lizard_core.file_info_builder import (  # noqa: E402
    NestingStack, FileInfoBuilder)
from lizard_core.file_analyzer import (  # noqa: E402
    preprocessing, comment_counter, line_counter, token_counter,
    condition_counter, FileAnalyzer)
from lizard_core.extensions import (  # noqa: E402
    get_extensions, print_extension_results)
from lizard_core.analysis import (  # noqa: E402
    analyze, analyze_files, map_files_to_analyzer, _map_in_pool,
    analyze_file)
from lizard_core.warning_filter import (  # noqa: E402
    DEFAULT_WHITELIST, warning_filter, whitelist_filter, get_warnings)
from lizard_core.output_scheme import OutputScheme  # noqa: E402
from lizard_core.text_output import (  # noqa: E402
    print_warnings, print_no_warnings, print_total, print_and_save_modules,
    print_result, silent_printer, print_clang_style_warning,
    print_msvs_style_warning)
from lizard_core.options import (  # noqa: E402
    DEFAULT_CCN_THRESHOLD, DEFAULT_MAX_FUNC_LENGTH, _extension_arg,
    arg_parser, parse_args, infer_printer_from_file_ext)
from lizard_core.cli import open_output_file, main  # noqa: E402


if __name__ == "__main__":
    sys.exit(main())
