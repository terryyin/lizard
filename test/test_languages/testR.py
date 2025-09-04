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


if __name__ == '__main__':
    unittest.main()
