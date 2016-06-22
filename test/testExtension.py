import unittest
from mock import Mock, patch
from lizard import get_extensions, FileAnalyzer
from lizard_ext.lizardio import LizardExtension as FanInOut
from lizard import OutputScheme, FileInformation, FunctionInfo
import importlib


class FakeExtension:
    def LizardExtension(self):
        return "fake extension"


@patch.object(importlib, 'import_module')
class Test_mounting_extensions(unittest.TestCase):
    def test_should_append_extension_at_the_end_by_default(self, mock_import):
        mock_import.return_value = FakeExtension()
        exts = get_extensions(["my_ext"])
        self.assertEqual("fake extension", exts[-1])

    def test_should_insert_extension_at_the_index_when_specified(self, mock_import):
        extension = Mock(ordering_index=1)
        del extension.AVERAGE_CAPTION
        mock_import.return_value = extension
        exts = get_extensions([extension])
        self.assertEqual(extension, exts[1])


class Test_using_extensions(unittest.TestCase):
    def setUp(self):
        self.func = FunctionInfo("foo", 'FILENAME', 100)
        self.file_info = FileInformation("filename", 10, [self.func])

    def test_should_generate_property_for_file_info(self):
        class MyExt:
            FUNCTION_INFO = {"max_nesting_depth": {"caption": "  ND  ", "average_caption": " Avg.ND "}}

        OutputScheme([MyExt()]).patch_for_extensions()
        self.func.max_nesting_depth = 1.5
        self.assertEqual(self.file_info.average_max_nesting_depth, 1.5)


class Test_using_muliple_base_extensions(unittest.TestCase):
    def setUp(self):
        self.ext = FanInOut()
        self.lizard_object = FileAnalyzer(get_extensions(
            ["io", "ns", "nd"])).analyze_source_code("a.cpp", """int foo(){
                                                                              bar();
                                                                              if(a){
                                                                                  b;
                                                                              }
                                                                          }
                                                              int bar(){foo();};""")

    def test_reduce_func_in_lizardio(self):
        lizard_object = None
        self.assertRaises((AttributeError, TypeError, ValueError), self.ext.reduce(lizard_object))


    def test_all_extention_simultaneously(self):

        self.ext.reduce(self.lizard_object)
        correct_max_nd = {'foo': 1,
                          'bar': 0}
        correct_max_nested_struct = {'foo': 1,
                                     'bar': 0}
        correct_fan_out = {'foo': 1,
                           'bar': 1}
        correct_general_fan_out = {'foo': 1,
                                   'bar': 1}
        correct_fan_in = {'foo': 1,
                          'bar': 1}
        self.assertDictEqual(correct_max_nd,
                             {func.name: func.max_nesting_depth for func in self.lizard_object.function_list})
        self.assertDictEqual(correct_max_nested_struct,
                             {func.name: func.max_nested_structures for func in self.lizard_object.function_list})
        self.assertDictEqual(correct_fan_out,
                             {func.name: func.fan_out for func in self.lizard_object.function_list})
        self.assertDictEqual(correct_general_fan_out,
                             {func.name: func.general_fan_out for func in self.lizard_object.function_list})
        self.assertDictEqual(correct_fan_in,
                             {func.name: func.fan_in for func in self.lizard_object.function_list})