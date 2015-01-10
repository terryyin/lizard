import unittest
from test.mock import Mock, patch
from lizard import get_extensions
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
        mock_import.return_value = extension
        exts = get_extensions([extension])
        self.assertEqual(extension, exts[1])

