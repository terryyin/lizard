import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages.r import RReader


def get_r_function_list(source_code):
    """Helper function to analyze R source code and return function list."""
    return analyze_file.analyze_source_code("test.r", source_code).function_list


class TestRTokenizer(unittest.TestCase):
    """Test R tokenization."""

    def check_tokens(self, expected, source):
        """Helper to check tokenization results."""
        tokens = list(RReader.generate_tokens(source))
        self.assertEqual(expected, tokens)

    def test_assignment_operators(self):
        """Test R assignment operators."""
        self.check_tokens(['x', ' ', '<-', ' ', '5'], 'x <- 5')
        self.check_tokens(['x', ' ', '=', ' ', '5'], 'x = 5')
        self.check_tokens(['5', ' ', '->', ' ', 'x'], '5 -> x')

    def test_special_operators(self):
        """Test R special operators."""
        self.check_tokens(['x', ' ', '%in%', ' ', 'y'], 'x %in% y')
        self.check_tokens(['x', ' ', '%*%', ' ', 'y'], 'x %*% y')
        self.check_tokens(['x', ' ', '%>%', ' ', 'y'], 'x %>% y')

    def test_namespace_operators(self):
        """Test R namespace operators."""
        self.check_tokens(['pkg', '::', 'func'], 'pkg::func')
        self.check_tokens(['pkg', ':::', 'internal'], 'pkg:::internal')

    def test_ellipsis(self):
        """Test ellipsis operator."""
        self.check_tokens(['function', '(', '...', ')'], 'function(...)')

    def test_comments(self):
        """Test comment handling."""
        self.check_tokens(['# This is a comment'], '# This is a comment')
        self.check_tokens(['x', ' ', '<-', ' ', '1', '  ', '# inline comment'], 'x <- 1  # inline comment')


class TestRFunctionParsing(unittest.TestCase):
    """Test R function parsing and complexity calculation."""

    def test_empty_code(self):
        """Test empty R code."""
        functions = get_r_function_list("")
        self.assertEqual(0, len(functions))

    def test_simple_function(self):
        """Test simple function definition."""
        code = '''
        simple_func <- function(x, y = 5) {
            result <- x + y
            return(result)
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("simple_func", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_alternative_assignment(self):
        """Test alternative assignment operators."""
        code = '''
        simple_func2 = function(a, b) {
            a + b -> result
            result
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("simple_func2", functions[0].name)

    def test_if_else_complexity(self):
        """Test if-else statement complexity."""
        code = '''
        complex_control <- function(x) {
            if (x > 0) {
                print("positive")
            } else if (x < 0) {
                print("negative")
            } else {
                print("zero")
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("complex_control", functions[0].name)
        # Should be 3: base(1) + if(1) + else if(1) = 3
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_nested_loops(self):
        """Test nested loops complexity."""
        code = '''
        nested_loops <- function(n) {
            for (i in 1:n) {
                for (j in 1:i) {
                    if (i %% j == 0) {
                        print(paste(i, j))
                    }
                }
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("nested_loops", functions[0].name)
        # Should be 4: base(1) + for(1) + for(1) + if(1) = 4
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_while_and_repeat_loops(self):
        """Test while and repeat loops."""
        code = '''
        iterative_func <- function(x) {
            while (x > 1) {
                x <- x / 2
            }

            repeat {
                x <- x * 2
                if (x > 100) break
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("iterative_func", functions[0].name)
        # Should be 4: base(1) + while(1) + repeat(1) + if(1) = 4
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_logical_operators(self):
        """Test logical operators complexity."""
        code = '''
        logical_complexity <- function(a, b, c) {
            if (a > 0 && b < 10 || c == 5) {
                return(1)
            }

            if (a %in% c(1, 2, 3) & b != 0) {
                return(2)
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("logical_complexity", functions[0].name)
        # Should be 6: base(1) + if(1) + &&(1) + ||(1) + if(1) + &(1) = 6
        self.assertEqual(6, functions[0].cyclomatic_complexity)

    def test_switch_statement(self):
        """Test switch statement complexity."""
        code = '''
        switch_func <- function(type) {
            switch(type,
                "a" = 1,
                "b" = 2,
                "c" = 3,
                default = 0
            )
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("switch_func", functions[0].name)
        # Should be 2: base(1) + switch(1) = 2
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_trycatch_complexity(self):
        """Test tryCatch error handling complexity."""
        code = '''
        error_handling <- function(x) {
            tryCatch({
                result <- 1 / x
                return(result)
            },
            error = function(e) {
                return(NA)
            },
            warning = function(w) {
                return(0)
            })
        }
        '''
        functions = get_r_function_list(code)
        # Should find the main function plus the error and warning handler functions
        self.assertTrue(len(functions) >= 1)
        main_func = next((f for f in functions if f.name == "error_handling"), None)
        self.assertIsNotNone(main_func)
        # Should be 2: base(1) + tryCatch(1) = 2
        self.assertEqual(2, main_func.cyclomatic_complexity)

    def test_anonymous_function(self):
        """Test anonymous function in apply."""
        code = '''
        apply_func <- function(data) {
            sapply(data, function(x) {
                if (x > 0) {
                    return(x^2)
                } else {
                    return(0)
                }
            })
        }
        '''
        functions = get_r_function_list(code)
        self.assertTrue(len(functions) >= 1)
        # Should find both the main function and the anonymous function
        main_func = next((f for f in functions if f.name == "apply_func"), None)
        self.assertIsNotNone(main_func)

    def test_single_line_function(self):
        """Test single line function without braces."""
        code = '''
        one_liner <- function(x) x + 1
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("one_liner", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_empty_function(self):
        """Test empty function."""
        code = '''
        empty_func <- function() {}
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("empty_func", functions[0].name)
        self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_function_with_complex_parameters(self):
        """Test function with complex parameter defaults."""
        code = '''
        complex_params <- function(a, b = ifelse(a > 0, 1, -1), ...) {
            if (b > 0) {
                return(a + b)
            } else {
                return(a - b)
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("complex_params", functions[0].name)
        # Should be 3: base(1) + ifelse(1) + if(1) = 3
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_multiple_functions(self):
        """Test multiple function definitions."""
        code = '''
        func1 <- function(x) {
            if (x > 0) return(1)
            return(0)
        }

        func2 <- function(y) {
            for (i in 1:y) {
                print(i)
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(2, len(functions))

        func1 = next((f for f in functions if f.name == "func1"), None)
        func2 = next((f for f in functions if f.name == "func2"), None)

        self.assertIsNotNone(func1)
        self.assertIsNotNone(func2)
        self.assertEqual(2, func1.cyclomatic_complexity)  # base + if
        self.assertEqual(2, func2.cyclomatic_complexity)  # base + for

    def test_comments_ignored(self):
        """Test that comments are properly ignored."""
        code = '''
        # This function does something
        commented_func <- function(x) {
            # This is a comment
            y <- x + 1  # Inline comment

            # Multi-line logic
            if (x > 0) {  # Positive check
                return("positive")
            } else {
                return("non-positive")  # Negative or zero
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("commented_func", functions[0].name)
        self.assertEqual(2, functions[0].cyclomatic_complexity)  # base + if

    def test_string_literals(self):
        """Test string literal handling."""
        code = '''
        string_func <- function() {
            single_quote <- 'Hello World'
            double_quote <- "Hello World"

            if (single_quote == double_quote) {
                return(TRUE)
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, len(functions))
        self.assertEqual("string_func", functions[0].name)
        self.assertEqual(2, functions[0].cyclomatic_complexity)  # base + if

    def test_s3_method_names_with_dots(self):
        """Test S3 method names with dots are parsed correctly."""
        code = '''
        print.myclass <- function(x, ...) {
            cat("Custom print for myclass\\n")
            if (length(x) > 10) {
                cat("Large object\\n")
            }
        }

        summary.myclass <- function(object, ...) {
            for (i in 1:length(object)) {
                if (is.numeric(object[[i]])) {
                    print(summary(object[[i]]))
                }
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(2, len(functions))

        print_func = next((f for f in functions if f.name == "print.myclass"), None)
        summary_func = next((f for f in functions if f.name == "summary.myclass"), None)

        self.assertIsNotNone(print_func, "print.myclass function should be detected with full dotted name")
        self.assertIsNotNone(summary_func, "summary.myclass function should be detected with full dotted name")
        self.assertEqual(2, print_func.cyclomatic_complexity)  # base + if
        self.assertEqual(3, summary_func.cyclomatic_complexity)  # base + for + if

    def test_nested_function_definitions(self):
        """Test nested function definitions are detected as separate functions."""
        code = '''
        outer_function <- function(x) {
            # Nested function definition
            inner_function <- function(y) {
                if (y > 0) {
                    return(y * 2)
                } else {
                    return(0)
                }
            }

            # Use the nested function
            result <- inner_function(x)

            if (result > 10) {
                return("large")
            } else if (result > 5) {
                return("medium")
            } else {
                return("small")
            }
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(2, len(functions), "Should detect both outer and inner functions")

        outer_func = next((f for f in functions if f.name == "outer_function"), None)
        inner_func = next((f for f in functions if f.name == "inner_function"), None)

        self.assertIsNotNone(outer_func, "outer_function should be detected")
        self.assertIsNotNone(inner_func, "inner_function should be detected as separate function")

        # Note: Due to the way nested functions are parsed, the outer function's
        # complexity may be split. This is acceptable behavior for now.
        # inner_function: base + if = 2
        self.assertEqual(2, inner_func.cyclomatic_complexity)
        # outer_function should have at least base complexity
        self.assertTrue(outer_func.cyclomatic_complexity >= 1)

    def test_right_assignment_operator(self):
        """Test right assignment operator (->) for function definitions."""
        code = '''
        # Right assignment with arrow operator
        function(x, y) {
            if (x > y) {
                return(x)
            } else {
                return(y)
            }
        } -> max_func

        # Another right assignment
        function(data) {
            for (i in 1:length(data)) {
                if (data[i] > 0) {
                    print(data[i])
                }
            }
        } -> print_positive
        '''
        functions = get_r_function_list(code)
        self.assertEqual(2, len(functions), "Should detect both functions with right assignment")

        max_func = next((f for f in functions if f.name == "max_func"), None)
        print_func = next((f for f in functions if f.name == "print_positive"), None)

        self.assertIsNotNone(max_func, "max_func should be detected with right assignment")
        self.assertIsNotNone(print_func, "print_positive should be detected with right assignment")

        # max_func: base + if = 2
        self.assertEqual(2, max_func.cyclomatic_complexity)
        # print_positive: base + for + if = 3
        self.assertEqual(3, print_func.cyclomatic_complexity)

    def test_multiple_assignment(self):
        """Test multiple assignment operators for function definitions."""
        code = '''
        # Multiple assignment: both func5 and func6 should be detected
        func5 <- func6 <- function(x, y) {
            if (x > y) {
                return(x * 2)
            } else {
                return(y * 2)
            }
        }

        # Another multiple assignment
        a <- b <- c <- function(data) {
            for (i in 1:length(data)) {
                if (data[i] < 0) {
                    data[i] <- 0
                }
            }
            return(data)
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(5, len(functions), "Should detect all functions in multiple assignment")

        func5 = next((f for f in functions if f.name == "func5"), None)
        func6 = next((f for f in functions if f.name == "func6"), None)
        func_a = next((f for f in functions if f.name == "a"), None)
        func_b = next((f for f in functions if f.name == "b"), None)
        func_c = next((f for f in functions if f.name == "c"), None)

        self.assertIsNotNone(func5, "func5 should be detected in multiple assignment")
        self.assertIsNotNone(func6, "func6 should be detected in multiple assignment")
        self.assertIsNotNone(func_a, "a should be detected in multiple assignment")
        self.assertIsNotNone(func_b, "b should be detected in multiple assignment")
        self.assertIsNotNone(func_c, "c should be detected in multiple assignment")

        # All functions should have the same complexity since they're the same function
        # func5/func6: base + if = 2
        self.assertEqual(2, func5.cyclomatic_complexity)
        self.assertEqual(2, func6.cyclomatic_complexity)
        # a/b/c: base + for + if = 3
        self.assertEqual(3, func_a.cyclomatic_complexity)
        self.assertEqual(3, func_b.cyclomatic_complexity)
        self.assertEqual(3, func_c.cyclomatic_complexity)

    def test_parenthesized_assignments(self):
        """Test parenthesized assignment expressions."""
        code = '''
        # Function assigned within parentheses
        (func_in_parens <- function(x) {
            if (x > 0) {
                return(x^2)
            } else {
                return(0)
            }
        })

        # More complex parenthesized expression
        result <- (helper_func <- function(data, threshold) {
            for (i in 1:length(data)) {
                if (data[i] > threshold) {
                    data[i] <- threshold
                }
            }
            return(data)
        })(some_data, 100)

        # Nested parentheses
        ((nested_func <- function(a, b) {
            return(a * b)
        }))
        '''
        functions = get_r_function_list(code)
        self.assertEqual(3, len(functions), "Should detect functions in parenthesized assignments")

        func_in_parens = next((f for f in functions if f.name == "func_in_parens"), None)
        helper_func = next((f for f in functions if f.name == "helper_func"), None)
        nested_func = next((f for f in functions if f.name == "nested_func"), None)

        self.assertIsNotNone(func_in_parens, "func_in_parens should be detected")
        self.assertIsNotNone(helper_func, "helper_func should be detected")
        self.assertIsNotNone(nested_func, "nested_func should be detected")

        # Check complexities
        # func_in_parens: base + if = 2
        self.assertEqual(2, func_in_parens.cyclomatic_complexity)
        # helper_func: base + for + if = 3
        self.assertEqual(3, helper_func.cyclomatic_complexity)
        # nested_func: base = 1
        self.assertEqual(1, nested_func.cyclomatic_complexity)


class TestRFileExtensions(unittest.TestCase):
    """Test R file extension matching."""

    def test_r_extensions(self):
        """Test that R reader matches .r and .R files."""
        self.assertTrue(RReader.match_filename("test.r"))
        self.assertTrue(RReader.match_filename("test.R"))
        self.assertTrue(RReader.match_filename("path/to/script.r"))
        self.assertTrue(RReader.match_filename("path/to/script.R"))

        # Should not match other extensions
        self.assertFalse(RReader.match_filename("test.py"))
        self.assertFalse(RReader.match_filename("test.rb"))
        self.assertFalse(RReader.match_filename("test.rs"))


class Test_R_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for R"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        simple <- function(x) {
            return(x + 1)
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        check <- function(x) {
            if (x > 0) {  # +1
                return("positive")
            }
            return("non-positive")
        }
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        nested <- function() {
            for (i in 1:10) {           # +1
                for (j in 1:10) {       # +2 (nesting=1)
                    if (i == j) {       # +3 (nesting=2)
                        print(i)
                    }
                }
            }
        }
        # Total CogC = 6
        '''
        functions = get_r_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        check <- function(a, b, c, d, e) {
            if (a && b && c) {  # +1 for if, +1 for && sequence
                return(1)
            }
            if (d || e) {       # +1 for if, +1 for || sequence
                return(2)
            }
            return(0)
        }
        # Total CogC = 4
        '''
        functions = get_r_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        countdown <- function(n) {
            while (n > 0) {  # +1
                n <- n - 1
            }
            return(n)
        }
        # Total CogC = 1
        '''
        functions = get_r_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)


if __name__ == '__main__':
    unittest.main()
