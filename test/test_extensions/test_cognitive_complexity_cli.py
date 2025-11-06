import unittest
from lizard import parse_args, FunctionInfo, FileInformation, get_warnings


class TestCognitiveComplexityCLI(unittest.TestCase):
    """Test CLI integration for Cognitive Complexity (-G/--CogC option)"""

    def test_cogc_default_threshold(self):
        """Test that default CogC threshold is 15 when extension is loaded"""
        options = parse_args(['lizard', '-Ecogc'])
        self.assertEqual(15, options.CogC)
        self.assertEqual(15, options.thresholds['cognitive_complexity'])

    def test_cogc_short_option(self):
        """Test -G option sets CogC threshold"""
        options = parse_args(['lizard', '-Ecogc', '-G20'])
        self.assertEqual(20, options.CogC)
        self.assertEqual(20, options.thresholds['cognitive_complexity'])

    def test_cogc_long_option(self):
        """Test --CogC option sets CogC threshold"""
        options = parse_args(['lizard', '-Ecogc', '--CogC', '25'])
        self.assertEqual(25, options.CogC)
        self.assertEqual(25, options.thresholds['cognitive_complexity'])

    def test_cogc_with_ccn_threshold(self):
        """Test that CogC and CCN thresholds work together"""
        options = parse_args(['lizard', '-Ecogc', '-C10', '-G20'])
        self.assertEqual(10, options.thresholds['cyclomatic_complexity'])
        self.assertEqual(20, options.thresholds['cognitive_complexity'])

    def test_cogc_threshold_via_T_option(self):
        """Test -T option can set cognitive_complexity threshold when extension is loaded"""
        options = parse_args(['lizard', '-Ecogc', '-Tcognitive_complexity=30'])
        self.assertEqual(30, options.thresholds['cognitive_complexity'])

    def test_cogc_T_option_overrides_G_option(self):
        """Test that -T option overrides -G option"""
        options = parse_args(['lizard', '-Ecogc', '-G20', '-Tcognitive_complexity=30'])
        self.assertEqual(30, options.thresholds['cognitive_complexity'])

    def test_cogc_sorting(self):
        """Test sorting by cognitive_complexity when extension is loaded"""
        options = parse_args(['lizard', '-Ecogc', '-scognitive_complexity'])
        self.assertEqual('cognitive_complexity', options.sorting[0])


class TestCognitiveComplexityWarningFilter(unittest.TestCase):
    """Test that CogC threshold filters warnings correctly"""

    def test_warning_when_cogc_exceeds_threshold(self):
        """Test warning is generated when CogC exceeds threshold"""
        options = parse_args(['lizard', '-Ecogc', '-G10'])
        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cognitive_complexity = 15  # Exceeds threshold of 10
        foo.cyclomatic_complexity = 5   # Below CCN threshold
        file_info = FileInformation("test.py", 123, [foo])

        warnings = list(get_warnings([file_info], options))
        self.assertEqual(1, len(warnings))
        self.assertEqual('foo', warnings[0].name)

    def test_no_warning_when_cogc_below_threshold(self):
        """Test no warning when CogC is below threshold"""
        options = parse_args(['lizard', '-Ecogc', '-G20'])
        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cognitive_complexity = 10  # Below threshold of 20
        foo.cyclomatic_complexity = 5   # Below CCN threshold
        file_info = FileInformation("test.py", 123, [foo])

        warnings = list(get_warnings([file_info], options))
        self.assertEqual(0, len(warnings))

    def test_warning_when_either_ccn_or_cogc_exceeds(self):
        """Test warning when either CCN or CogC exceeds threshold"""
        options = parse_args(['lizard', '-Ecogc', '-C10', '-G10'])

        # Function with high CCN but low CogC
        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cyclomatic_complexity = 15
        foo.cognitive_complexity = 5

        # Function with low CCN but high CogC
        bar = FunctionInfo("bar", 'test.py', 10)
        bar.cyclomatic_complexity = 5
        bar.cognitive_complexity = 15

        file_info = FileInformation("test.py", 123, [foo, bar])
        warnings = list(get_warnings([file_info], options))

        self.assertEqual(2, len(warnings))
        self.assertIn('foo', [w.name for w in warnings])
        self.assertIn('bar', [w.name for w in warnings])

    def test_warning_sorting_by_cogc(self):
        """Test warnings are sorted by CogC correctly"""
        options = parse_args(['lizard', '-Ecogc', '-G5', '-scognitive_complexity'])

        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cognitive_complexity = 10
        foo.cyclomatic_complexity = 20  # High CCN

        bar = FunctionInfo("bar", 'test.py', 10)
        bar.cognitive_complexity = 20  # Higher CogC
        bar.cyclomatic_complexity = 10

        baz = FunctionInfo("baz", 'test.py', 20)
        baz.cognitive_complexity = 15
        baz.cyclomatic_complexity = 15

        file_info = FileInformation("test.py", 123, [foo, bar, baz])
        warnings = list(get_warnings([file_info], options))

        # Should be sorted by CogC: bar (20), baz (15), foo (10)
        self.assertEqual(3, len(warnings))
        self.assertEqual('bar', warnings[0].name)
        self.assertEqual('baz', warnings[1].name)
        self.assertEqual('foo', warnings[2].name)

    def test_cogc_exact_threshold_no_warning(self):
        """Test no warning when CogC equals threshold (not greater than)"""
        options = parse_args(['lizard', '-Ecogc', '-G15'])
        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cognitive_complexity = 15  # Equals threshold
        foo.cyclomatic_complexity = 5
        file_info = FileInformation("test.py", 123, [foo])

        warnings = list(get_warnings([file_info], options))
        self.assertEqual(0, len(warnings))

    def test_cogc_threshold_plus_one_warning(self):
        """Test warning when CogC is threshold + 1"""
        options = parse_args(['lizard', '-Ecogc', '-G15'])
        foo = FunctionInfo("foo", 'test.py', 1)
        foo.cognitive_complexity = 16  # One more than threshold
        foo.cyclomatic_complexity = 5
        file_info = FileInformation("test.py", 123, [foo])

        warnings = list(get_warnings([file_info], options))
        self.assertEqual(1, len(warnings))
