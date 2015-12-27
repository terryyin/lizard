import unittest
import sys


class StreamStdoutTestCase(unittest.TestCase):
    def setUp(self):
        self.savedStdout = sys.stdout
        sys.stdout = self.StreamForTest()

    def tearDown(self):
        sys.stdout = self.savedStdout

    class StreamForTest:

        def __init__(self):
            self.stream = ""

        def write(self, x):
            self.stream += str(x)

        def __getattr__(self, attr):
            return getattr(self.stream, attr)
