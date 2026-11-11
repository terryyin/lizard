import unittest

from lizard import analyze_file
from lizard_languages import GherkinReader, get_reader_for


SEED_EXAMPLE_1 = '''\
Feature: basket
  Background: common
    Given setup

  Scenario: first
    Given a
    When b

  Scenario Outline: second
    Given x
    When y
    Examples:
      | a |
      | 1 |
      | 2 |
'''


def analyze_feature(source):
    return analyze_file.analyze_source_code('basket.feature', source)


class TestGherkinRegistration(unittest.TestCase):

    def test_feature_extension_selects_gherkin_reader(self):
        self.assertIs(GherkinReader, get_reader_for('basket.feature'))
        self.assertIs(GherkinReader, get_reader_for('Basket.FEATURE'))


class TestGherkinSeedExample1(unittest.TestCase):

    def test_written_blocks_have_expected_nloc_ccn_and_ranges(self):
        result = analyze_feature(SEED_EXAMPLE_1)
        self.assertEqual(
            [('Background: common', 2, 1, 2, 3),
             ('Scenario: first', 3, 1, 5, 7),
             ('Scenario Outline: second', 7, 1, 9, 15)],
            [(f.name, f.nloc, f.cyclomatic_complexity, f.start_line, f.end_line)
             for f in result.function_list])


class TestGherkinOwnership(unittest.TestCase):

    def test_background_is_not_copied_into_scenarios(self):
        code = '''\
Feature: share
  Background: common
    Given setup
    And more setup
  Scenario: one
    Given a
  Scenario: two
    Given b
'''
        result = analyze_feature(code)
        self.assertEqual(
            [3, 2, 2],
            [f.nloc for f in result.function_list])
        self.assertEqual(
            ['Background: common', 'Scenario: one', 'Scenario: two'],
            [f.name for f in result.function_list])

    def test_tagged_examples_enlarge_only_their_outline(self):
        code = '''\
Feature: outlines
  Scenario Outline: priced
    Given a <item>
    Examples:
      | item |
      | apple |
    @fast
    Examples:
      | item |
      | pear |
  Scenario: other
    Given done
'''
        result = analyze_feature(code)
        self.assertEqual(2, len(result.function_list))
        outline, other = result.function_list
        self.assertEqual('Scenario Outline: priced', outline.name)
        self.assertEqual(9, outline.nloc)
        self.assertEqual(1, outline.cyclomatic_complexity)
        self.assertEqual(('Scenario: other', 2), (other.name, other.nloc))

    def test_description_comment_blank_and_step_punctuation(self):
        code = '''\
Feature: noise
  Scenario: noisy
    a plain description

    # full line comment
    Given text with # if && punctuation
  @keep
  Scenario: next
    Given clean
'''
        result = analyze_feature(code)
        self.assertEqual(2, len(result.function_list))
        first, second = result.function_list
        self.assertEqual(3, first.nloc)
        self.assertEqual(1, first.cyclomatic_complexity)
        self.assertEqual((2, 6), (first.start_line, first.end_line))
        self.assertEqual(2, second.nloc)
        self.assertEqual(7, result.nloc)

    def test_doc_strings_and_table_cells_are_opaque(self):
        for delimiter in ('"""', '```'):
            code = f'''\
Feature: literals
  Scenario: quoted
    Given payload
      {delimiter}
      Scenario: quoted data
      # literal data

      {delimiter}
    And table
      | Scenario: cell |
      | # also data |
'''
            result = analyze_feature(code)
            self.assertEqual(1, len(result.function_list), delimiter)
            function = result.function_list[0]
            self.assertEqual('Scenario: quoted', function.name)
            self.assertEqual(9, function.nloc)
            self.assertEqual(1, function.cyclomatic_complexity)

    def test_rule_feature_tags_aliases_unnamed_duplicates_eof(self):
        code = '''\
Feature: outer
  Scenario: before
    Given x
  Rule: grouping
    description of rule
    Background:
      Given shared
    Example: inside
      Given y
  Scenario Template: again
    Given z
    Scenarios:
      | n |
      | 1 |
  Scenario: before
    Given duplicate title
'''
        # no trailing newline on purpose
        code = code.rstrip('\n')
        result = analyze_feature(code)
        names = [f.name for f in result.function_list]
        self.assertEqual(
            ['Scenario: before',
             'Background',
             'Example: inside',
             'Scenario Template: again',
             'Scenario: before'],
            names)
        before, background, example, template, duplicate = result.function_list
        self.assertEqual(2, before.nloc)
        self.assertEqual(3, before.end_line)
        self.assertEqual(2, background.nloc)
        self.assertEqual('Background', background.name)
        self.assertEqual(2, example.nloc)
        self.assertEqual(5, template.nloc)
        self.assertEqual(2, duplicate.nloc)
        self.assertNotEqual(before.start_line, duplicate.start_line)
        self.assertTrue(all(f.cyclomatic_complexity == 1 for f in result.function_list))

    def test_empty_and_container_only_have_no_functions(self):
        self.assertEqual([], analyze_feature('').function_list)
        self.assertEqual(
            [],
            analyze_feature('Feature: only\n  Rule: still\n').function_list)
