import re
import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_ext import lizardnonstrict


IF_AND_READY = """
CREATE OR REPLACE PROCEDURE If_And_Ready(p_And NUMBER, y NUMBER) IS
BEGIN
  <<outer_loop>>
  LOOP
    EXIT WHEN p_And > 9;
    IF p_And > 0 AND y > 0 THEN
      NULL;
    ELSIF p_And < 0 OR y < 0 THEN
      NULL;
    END IF;
  END LOOP outer_loop;
  WHILE y > 0 AND NOT p_And = 1 LOOP
    NULL;
  END LOOP;
END;
"""

FOR_CASE = """
CREATE OR REPLACE PROCEDURE For_Case(p_n NUMBER) IS
BEGIN
  FOR i IN 1..3 LOOP
    NULL;
  END LOOP;
  CASE p_n
    WHEN 1 THEN NULL;
    WHEN 2 THEN NULL;
    ELSE NULL;
  END CASE;
END;
"""

CALC = """
CREATE OR REPLACE FUNCTION Calc(p_Flag BOOLEAN DEFAULT TRUE OR FALSE,
                                p_For NUMBER) RETURN NUMBER IS
  v NUMBER := CASE WHEN p_For > 1 THEN 1 ELSE 0 END;
BEGIN
  IF p_Flag AND v > 0 THEN RETURN 1; END IF;
  RETURN 0;
END;
"""

KEYWORDS = (
    "CREATE", "OR", "REPLACE", "PROCEDURE", "NUMBER", "IS", "BEGIN", "LOOP",
    "EXIT", "WHEN", "IF", "AND", "THEN", "NULL", "ELSIF", "END", "WHILE",
    "NOT", "FOR", "IN", "CASE", "ELSE", "FUNCTION", "BOOLEAN", "DEFAULT",
    "TRUE", "FALSE", "RETURN")

CASINGS = {
    "upper": str.upper, "lower": str.lower, "capitalized": str.capitalize}


def in_casing(source, casing):
    return re.sub(r"\b(%s)\b" % "|".join(KEYWORDS),
                  lambda match: casing(match.group(0)), source)


def default_function(source):
    function, = analyze_file.analyze_source_code(
        'sample.sql', source).function_list
    return function


def nonstrict_function(source):
    analyzer = FileAnalyzer(get_extensions([lizardnonstrict.LizardExtension()]))
    function, = analyzer.analyze_source_code('sample.sql', source).function_list
    return function


class TestMixedCasePLSQLConditions(unittest.TestCase):
    def test_keyword_case_does_not_change_complexity(self):
        bodies = [
            ('If x aNd y oR z Then NULL; ElsIf y Then NULL; End If;', 5),
            ('For i In 1..5 LoOp NULL; End LoOp;', 2),
            ('WhIlE ready LoOp NULL; End LoOp;', 2),
            ('CaSe x WhEn 1 ThEn NULL; WhEn 2 ThEn NULL; End CaSe;', 3),
            ("value := 'If And Or While';", 1),
        ]
        for body, complexity in bodies:
            with self.subTest(body=body):
                source = 'Create Procedure MixedName As Begin ' + body + ' End;'
                function = default_function(source)
                self.assertEqual('MixedName', function.name)
                self.assertEqual(complexity, function.cyclomatic_complexity)

    def test_boolean_operators_count_by_default_in_every_casing(self):
        for name, casing in CASINGS.items():
            with self.subTest(casing=name):
                function = default_function(in_casing(IF_AND_READY, casing))
                self.assertEqual(8, function.cyclomatic_complexity)

    def test_nonstrict_drops_boolean_operators_in_every_casing(self):
        for name, casing in CASINGS.items():
            with self.subTest(casing=name):
                function = nonstrict_function(in_casing(IF_AND_READY, casing))
                self.assertEqual(5, function.cyclomatic_complexity)

    def test_identifiers_containing_keywords_keep_their_spelling(self):
        for name, casing in CASINGS.items():
            with self.subTest(casing=name):
                function = nonstrict_function(in_casing(IF_AND_READY, casing))
                self.assertEqual(['p_And', 'y'], function.parameters)
                self.assertTrue(
                    function.long_name.startswith('If_And_Ready(p_And '))

    def test_for_loop_and_case_count_the_same_in_every_casing_and_mode(self):
        for name, casing in CASINGS.items():
            source = in_casing(FOR_CASE, casing)
            for mode, analyze in (('default', default_function),
                                  ('nonstrict', nonstrict_function)):
                with self.subTest(casing=name, mode=mode):
                    function = analyze(source)
                    self.assertEqual(4, function.cyclomatic_complexity)

    def test_long_name_keeps_the_source_spelling_of_keywords(self):
        for name in ('upper', 'capitalized'):
            with self.subTest(casing=name):
                source = in_casing(CALC, CASINGS[name])
                function = default_function(source)
                self.assertEqual(
                    in_casing('Calc(p_Flag BOOLEAN DEFAULT TRUE OR FALSE, '
                              'p_For NUMBER)', CASINGS[name]),
                    function.long_name)
