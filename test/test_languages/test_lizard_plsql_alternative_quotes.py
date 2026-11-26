import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN q'[it's if (x) "
          "then end if;]'; END;\n"
          'CREATE PROCEDURE g AS BEGIN NULL; END;',
          [('f', 1), ('g', 1)]),
         ("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN Q'{it's if x "
          "then end if;}'; END;\n"
          'CREATE PROCEDURE g AS BEGIN NULL; END;',
          [('f', 1), ('g', 1)]),
         ("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN q'(it's if x "
          "then end if;)'; END;\n"
          'CREATE PROCEDURE g AS BEGIN NULL; END;',
          [('f', 1), ('g', 1)]),
         ("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN q'<it's if x "
          "then end if;>'; END;\n"
          'CREATE PROCEDURE g AS BEGIN NULL; END;',
          [('f', 1), ('g', 1)]),
         ("CREATE FUNCTION f RETURN varchar2 AS BEGIN RETURN q'#it's if x "
          "then end if;#'; END;\n"
          'CREATE PROCEDURE g AS BEGIN NULL; END;',
          [('f', 1), ('g', 1)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.sql", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])

    def test_national_character_prefix(self):
        for literal in ("nq'[it's if x then end if;]'",
                        "Nq'#it's if x then end if;#'",
                        "NQ'(it's if x then end if;)'",
                        "nQ'{it's if x then end if;}'"):
            with self.subTest(literal=literal):
                source = ("CREATE FUNCTION f RETURN varchar2 AS BEGIN\n"
                          "  IF y THEN NULL; END IF;\n"
                          "  RETURN " + literal + ";\nEND;\n"
                          "CREATE PROCEDURE g AS BEGIN NULL; END;")
                result = analyze_file.analyze_source_code("sample.sql", source)
                self.assertEqual([('f', 2, 1, 4), ('g', 1, 5, 5)],
                                 [(f.name, f.cyclomatic_complexity,
                                   f.start_line, f.end_line)
                                  for f in result.function_list])
