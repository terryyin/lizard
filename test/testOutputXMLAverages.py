"""The cppncss function measure has one final average per metric."""
import unittest
from xml.etree import ElementTree

from lizard import AllResult, analyze_file
from lizard_ext.xmloutput import xml_output


class TestXMLFunctionAverages(unittest.TestCase):
    def measure(self, sources):
        files = [analyze_file.analyze_source_code(name, source)
                 for name, source in sources]
        document = ElementTree.fromstring(xml_output(AllResult(files), False))
        return document.find("./measure[@type='Function']")

    def test_multifile_average_is_reported_once_after_all_functions(self):
        measure = self.measure([
            ('one.c', 'int one() { return 1; }'),
            ('two.c', 'int two(int x) {\nif (x) return 2;\nreturn 0;\n}')])
        averages = measure.findall('average')
        self.assertEqual(2, len(averages))
        self.assertEqual({'NCSS': 2.5, 'CCN': 1.5},
                         {item.get('label'): float(item.get('value'))
                          for item in averages})
        self.assertEqual(['labels', 'item', 'item', 'average', 'average'],
                         [element.tag for element in measure])

    def test_empty_files_do_not_duplicate_existing_averages(self):
        measure = self.measure([
            ('one.c', 'int one() { return 1; }'),
            ('empty.c', '// no functions\n')])
        self.assertEqual(2, len(measure.findall('average')))

    def test_no_functions_have_no_function_averages(self):
        for sources in ([], [('empty.c', '// no functions\n')]):
            with self.subTest(sources=sources):
                self.assertEqual([], self.measure(sources).findall('average'))
