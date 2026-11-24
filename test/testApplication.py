import unittest
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch
import lizard
import os
import sys


@patch('lizard_core.warning_filter.auto_read', return_value='')
@patch('lizard_core.file_discovery.auto_read', return_value='')
@patch('lizard_core.file_discovery.md5_hash_file')
@patch('lizard_core.file_analyzer.auto_read')
@patch.object(os, 'walk')
@patch('lizard_core.cli.print_result')
class TestApplication(unittest.TestCase):

    def testEmptyResult(self, print_result, os_walk, mock_open, _, _gitignore,
                        _whitelist):

        def check_empty_result(result, options, scheme, _):
            self.assertEqual([], list(result))
            return 0

        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = check_empty_result
        lizard.main(['lizard'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open, _,
                              _gitignore, _whitelist):
        def check_result(result, options, scheme, _):
            fileInfos = list(result)
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
            return 0
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard.main(['lizard'])


class IntegrationTests(unittest.TestCase):

    def setUp(self):
        self.source_code = '''
        void foo() {
        #if
        #endif
            if(bar)
            {
            }
            switch(bar)
            {
              case 0: break;
              case 1: break;
              case 2: break;
              case 3: break;
            }
        }
        '''
        self.returned_warning_count = 0

    @patch('lizard_core.warning_filter.auto_read', return_value='')
    @patch('lizard_core.file_discovery.auto_read', return_value='')
    @patch('lizard_core.file_analyzer.auto_read')
    @patch('lizard_core.cli.print_result')
    def run_with_mocks(self, argv, src, print_result, mock_open, _gitignore,
                       _whitelist):
        def store_result(result, options, scheme, _):
            self.fileInfos = list(result)
            return self.returned_warning_count
        mock_open.return_value = src
        print_result.side_effect = store_result
        self.exit_code = lizard.main(argv)
        return self.fileInfos

    @patch('lizard_core.file_discovery.md5_hash_file')
    @patch.object(os, 'walk')
    def runApplicationWithArgv(self, argv, os_walk, _):
        os_walk.return_value = [('.', [], ['a.cpp'])]
        return self.run_with_mocks(argv, self.source_code)

    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard'])
        self.assertEqual(7, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    @patch('webbrowser.open')
    def test_using_the_WordCount_plugin(self, webopen):
        self.runApplicationWithArgv(['lizard', '-EWordCount'])
        self.assertEqual(1, self.fileInfos[0].wordCount["foo"])

    def test_using_modified_ccn(self):
        self.runApplicationWithArgv(['lizard', '--modified'])
        self.assertEqual(4, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_exit_code(self):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-C5'])
        self.assertEqual(self.exit_code, 1)
        self.runApplicationWithArgv(['lizard', '-i5'])
        self.assertEqual(self.exit_code, 1)

    def test_exit_code_ignore_warnings(self):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-i6'])
        self.assertEqual(self.exit_code, 0)
        self.runApplicationWithArgv(['lizard', '-i', '-1'])
        self.assertEqual(self.exit_code, 0)

    def test_lizard_script_runs_with_version(self):
        """Test lizard.py runs correctly when executed as script (issue #460)."""
        lizard_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        lizard_py = os.path.join(lizard_dir, 'lizard.py')
        result = subprocess.run(
            [sys.executable, lizard_py, '--version'],
            capture_output=True,
            text=True,
            cwd=lizard_dir,
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr or result.stdout)


class TestVueSourceLineNumbersCli(unittest.TestCase):
    """Real CLI checks for Vue original source locations (not mocked)."""

    VUE_SOURCE = '''\
<template>
  <div>
    Hello
  </div>
</template>

<script>
function first() {
  if (x) return 1;
  return 2;
}

function second() {
  return 3;
}
</script>
'''

    def setUp(self):
        self.lizard_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        self.lizard_py = os.path.join(self.lizard_dir, 'lizard.py')

    def test_default_output_and_warning_use_original_vue_lines(self):
        with TemporaryDirectory() as directory:
            vue_path = Path(directory) / 'component.vue'
            vue_path.write_text(self.VUE_SOURCE)

            default = subprocess.run(
                [sys.executable, self.lizard_py, str(vue_path)],
                capture_output=True,
                text=True,
                cwd=self.lizard_dir,
            )
            self.assertEqual(default.returncode, 0, msg=default.stderr or default.stdout)
            self.assertEqual('', default.stderr)
            self.assertIn('first@8-11@' + str(vue_path), default.stdout)
            self.assertIn('second@13-15@' + str(vue_path), default.stdout)

            warning = subprocess.run(
                [sys.executable, self.lizard_py, '-w', '-C1', str(vue_path)],
                capture_output=True,
                text=True,
                cwd=self.lizard_dir,
            )
            self.assertEqual(warning.returncode, 1, msg=warning.stderr or warning.stdout)
            self.assertIn(
                '%s:8: warning: first has' % vue_path,
                warning.stdout,
            )


class TestWarningsToStderrCli(unittest.TestCase):

    def setUp(self):
        self.directory = TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.source = self.root / 'warning.c'
        self.source.write_text('int branch(int x) { if (x) return 1; return 0; }\n')
        self.lizard_py = Path(__file__).resolve().parents[1] / 'lizard.py'

    def run_lizard(self, *options):
        return subprocess.run(
            [sys.executable, str(self.lizard_py), '-C1', *options,
             str(self.source)],
            capture_output=True, text=True, cwd=self.root)

    def test_default_report_routes_only_warning_section(self):
        default = self.run_lizard()
        self.assertEqual(default.returncode, 1, default.stderr)
        self.assertEqual(default.stderr, '')
        self.assertIn('!!!! Warnings', default.stdout)

        redirected = self.run_lizard('--warnings-to-stderr')
        self.assertEqual(redirected.returncode, 1, redirected.stderr)
        self.assertIn('!!!! Warnings', redirected.stderr)
        self.assertIn('branch@1-1@' + str(self.source), redirected.stderr)
        self.assertNotIn('!!!! Warnings', redirected.stdout)
        self.assertIn('1 file analyzed.', redirected.stdout)
        self.assertIn('Total nloc', redirected.stdout)

    def test_warning_section_remains_visible_with_output_file(self):
        report = self.root / 'report.txt'
        result = self.run_lizard('--warnings-to-stderr', '-o', str(report))
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stdout, '')
        self.assertIn('!!!! Warnings', result.stderr)
        self.assertIn('branch@1-1@' + str(self.source), result.stderr)
        self.assertIn('1 file analyzed.', report.read_text())
        self.assertIn('Total nloc', report.read_text())
        self.assertNotIn('!!!! Warnings', report.read_text())

    def test_success_without_warnings_leaves_stderr_empty(self):
        result = self.run_lizard('--warnings-to-stderr', '-C2')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, '')
        self.assertIn('No thresholds exceeded', result.stdout)

    def test_ignored_warnings_keep_the_success_exit_code(self):
        result = self.run_lizard('--warnings-to-stderr', '-i1')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('!!!! Warnings', result.stderr)

    def test_warning_only_formats_use_stderr(self):
        for option in ('-w', '--warning-msvs'):
            with self.subTest(option=option):
                default = self.run_lizard(option)
                result = self.run_lizard(option, '--warnings-to-stderr')
                self.assertEqual(default.returncode, 1, default.stderr)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertEqual(default.stderr, '')
                self.assertEqual(result.stdout, '')
                self.assertIn('branch', result.stderr)
                self.assertEqual(result.stderr, default.stdout)

    def test_structured_report_keeps_its_output_stream(self):
        default = self.run_lizard('--csv')
        result = self.run_lizard('--csv', '--warnings-to-stderr')
        self.assertEqual(result.returncode, default.returncode)
        self.assertEqual(result.stderr, '')
        self.assertEqual(result.stdout, default.stdout)
        self.assertIn('branch', result.stdout)


class TestHeaderDiscovery(unittest.TestCase):

    def test_directory_analysis_includes_hh_header_functions(self):
        with TemporaryDirectory() as directory:
            header = Path(directory) / 'sample.hh'
            header.write_text('''
inline int clamp(int value) {
    if (value < 0) return 0;
    return value;
}
''')

            results = list(lizard.analyze([directory]))

            self.assertEqual([str(header)], [info.filename for info in results])
            self.assertEqual(
                [('clamp', 2)],
                [(function.name, function.cyclomatic_complexity)
                 for function in results[0].function_list])
