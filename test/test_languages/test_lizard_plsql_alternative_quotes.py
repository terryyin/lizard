import unittest
from lizard import analyze_file


FOLLOWING_PROCEDURE = 'CREATE PROCEDURE g AS BEGIN NULL; END;'


def function_returning(literal):
    return ("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN " + literal +
            "; END;\n" + FOLLOWING_PROCEDURE)


def analyze(source):
    return [(f.name, f.cyclomatic_complexity, f.start_line)
            for f in analyze_file.analyze_source_code(
                "sample.sql", source).function_list]


class TestPLSQLAlternativeQuotes(unittest.TestCase):

    def test_alternative_quoted_literal_is_opaque(self):
        literals = [
            "q'[it's if (x) then end if;]'",
            "Q'{it's if x then end if;}'",
            "q'(it's if x then end if;)'",
            "q'<it's if x then end if;>'",
            "q'#it's if x then end if;#'",
        ]
        for literal in literals:
            with self.subTest(literal=literal):
                self.assertEqual([('f', 1, 1), ('g', 1, 2)],
                                 analyze(function_returning(literal)))

    def test_national_alternative_quoted_literal_is_opaque(self):
        literals = [
            "nq'[it's if x then end if;]'",
            "NQ'{it's if x then end if;}'",
            "nq'(it's if x then end if;)'",
            "nq'<it's if x then end if;>'",
            "Nq'#it's if x then end if;#'",
            "nQ'!it's if x then end if;!'",
        ]
        for literal in literals:
            with self.subTest(literal=literal):
                self.assertEqual([('f', 1, 1), ('g', 1, 2)],
                                 analyze(function_returning(literal)))

    def test_multiline_national_literal_keeps_following_lines(self):
        literal = "nq'[it's\nif x then end if;\n]'"
        self.assertEqual([('f', 1, 1), ('g', 1, 4)],
                         analyze(function_returning(literal)))

    def test_national_string_is_not_alternative_quoted(self):
        source = ("CREATE FUNCTION f RETURN varchar2 AS BEGIN "
                  "IF x THEN NULL; END IF; RETURN N'it''s'; END;\n" +
                  FOLLOWING_PROCEDURE)
        self.assertEqual([('f', 2, 1), ('g', 1, 2)], analyze(source))

    def test_identifier_named_nq_is_not_alternative_quoted(self):
        source = ("CREATE FUNCTION f RETURN NUMBER AS nq NUMBER := 2; "
                  "BEGIN IF nq > 1 THEN RETURN nq; END IF; RETURN 0; END;\n" +
                  FOLLOWING_PROCEDURE)
        self.assertEqual([('f', 2, 1), ('g', 1, 2)], analyze(source))
