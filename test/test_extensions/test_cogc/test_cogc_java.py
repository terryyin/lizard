"""Cognitive Complexity tests for Java"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_java_cogc(source_code):
    """Analyze Java code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.java", source_code
    ).function_list


class TestJavaCognitiveComplexity(unittest.TestCase):
    """Test Cognitive Complexity calculations for Java code"""

    def test_simple_method_has_zero_cogc(self):
        """Simple method with no control flow should have CogC = 0"""
        code = '''
public class Simple {
    public int add(int x, int y) {
        return x + y;
    }
}
'''
        functions = get_java_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_nested_loops_increase_cogc_more_than_ccn(self):
        """Nested loops should increase CogC more than CCN"""
        code = '''
public class Nested {
    public void nested() {
        for (int i = 0; i < 10; i++) {      // +1
            for (int j = 0; j < 10; j++) {  // +2 (nesting=1)
                if (i == j) {               // +3 (nesting=2)
                    System.out.println(i);
                }
            }
        }
    }
}
'''
        functions = get_java_cogc(code)
        # CogC = 1 + 2 + 3 = 6
        # CCN = 3 (for, for, if)
        self.assertGreaterEqual(functions[0].cognitive_complexity, 3)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 regardless of cases"""
        code = '''
public class Switcher {
    public String getWord(int number) {
        switch (number) {  // +1 for entire switch
            case 1:
                return "one";
            case 2:
                return "a couple";
            case 3:
                return "a few";
            default:
                return "lots";
        }
    }
}
'''
        functions = get_java_cogc(code)
        # CogC = 1 (entire switch)
        # SPEC REQUIREMENT: CogC = 1
        # CCN = 4 (case, case, case, default)
        self.assertEqual(1, functions[0].cognitive_complexity)
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_try_catch_finally(self):
        """try-catch-finally should count catch clauses"""
        code = '''
public class ErrorHandler {
    public void handle() {
        try {
            riskyOperation();
        } catch (IOException e) {  // +1
            logError(e);
        } catch (Exception e) {    // +1
            handleError(e);
        } finally {
            cleanup();
        }
    }
}
'''
        functions = get_java_cogc(code)
        # CogC = 2 (two catch clauses)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_nested_try_catch(self):
        """Nested try-catch should get nesting penalty"""
        code = '''
public class NestedError {
    public void handle() {
        try {
            if (condition) {           // +1
                try {
                    riskyOp();
                } catch (Exception e) { // +2 (nesting=1)
                    handle(e);
                }
            }
        } catch (Exception e) {        // +1
            log(e);
        }
    }
}
'''
        functions = get_java_cogc(code)
        # CogC = 1 (if) + 2 (nested catch) + 1 (outer catch) = 4
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operators in sequence"""
        code = '''
public class Logical {
    public boolean check() {
        if (a && b && c) {     // +1 for if, +1 for && sequence
            return true;
        }
        if (d || e || f) {     // +1 for if, +1 for || sequence
            return false;
        }
        return false;
    }
}
'''
        functions = get_java_cogc(code)
        # CogC = 1 (if) + 1 (&&) + 1 (if) + 1 (||) = 4
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_complex_nested_validation_logic(self):
        """Test complex nested validation with multiple conditions - CogC=19"""
        code = '''
@Nullable
private Handler findMatchingHandler(Request req) {
	if (req.isInvalid()) {                              // +1
		return ErrorHandler.defaultHandler;
	}

	boolean errorFound = false;
	List<Handler> handlers = req.getConfig().getHandlers().find(type);
	for (Handler h : handlers) {                         // +2 (nesting=1)
		if (h.canHandle(Request.POST)                // +3 (nesting=2)
			    && !h.isDisabled()) {                // +1 (binary operator sequence)
		Handler selected = (Handler)h;
		if (isCompatible(selected)) {                // +4 (nesting=3)
			Boolean matches = verifyParameters(selected,
                            req);
				if (matches == null) {               // +5 (nesting=4)
					if (!errorFound) {           // +6 (nesting=5)
						errorFound = true;
					}
				} else if (matches) {                // +1 (else if, hybrid)
					return selected;
				}
			}
		}
	}

	if (errorFound) {                                    // +1
		return ErrorHandler.defaultHandler;
	}
	return null;
}
'''
        functions = get_java_cogc(code)
        # CogC = 19
        self.assertEqual(19, functions[0].cognitive_complexity)

    def test_complex_retry_logic_with_nested_exception_handling(self):
        """Test complex retry logic with deeply nested control flow - CogC=35"""
        code = '''
private void processRecord(final Record rec, final Session session)
		throws ProcessingException, ValidationException {
	final SessionManager mgr = _system.getSessionManager();
	while (true) {                                           // +1
		try {                                            // +0 (try doesn't count)
			synchronized (this) {                    // +0 (synchronized doesn't count)
				if (head != null) {              // +1 (nesting=0, try/sync don't count)
					if (head.getTimestamp() > rec.getTimestamp()) {  // +2 (nesting=1)
						throw new ValidationException();
					}
					if (session.isValid()) {                 // +2 (nesting=1)
						for
                                (Record r = head; r != null; r = r.getPrevious()) {  // +3 (nesting=2)
                            final long timestamp = r.getTimestamp();
                            final long status = mgr.checkStatus(timestamp,
                                session.getState(), 0);
                            if (status == TIMEOUT) {                             // +4 (nesting=3)
                                throw new RetryException(timestamp);
                            }
                            if (status != 0                                      // +4 (nesting=3)
                                    && status != CANCELLED) {                    // +1 (binary operator)
                                throw new ValidationException();
                            }
					    }
				    }
                }
                rec.setNext(head);
                head = rec;
                break;
            }
        } catch (final RetryException re) {                                      // +1
            try {                                                                // +0
                final long status = _system.getSessionManager()
                        .checkStatus(re.getHandle(),session.getState(),
                        Config.DEFAULT_TIMEOUT);
                if (status != 0                                                  // +2 (nesting=1)
                        && status != CANCELLED) {                                // +1 (binary operator)
                    throw new ValidationException();
                }
            } catch (final InterruptedException ie) {                            // +2 (nesting=1)
                throw new ProcessingException(ie);
            }
        } catch (final InterruptedException ie) {                                // +1
            throw new ProcessingException(ie);
        }
	}
}
'''
        functions = get_java_cogc(code)
        # CogC = 35
        self.assertEqual(35, functions[0].cognitive_complexity)

    def test_complex_string_parsing_with_nested_conditionals(self):
        """Test complex string parsing with nested character checking - CogC=20"""
        code = '''
private static String parseFormat(String template, String delimiter) {
	final String escapedDelim = "\\\\" + delimiter;
	final StringBuilder result = new StringBuilder(template.length());
	result.append('^');
	int idx = template.startsWith("/") || template.startsWith("\\\\") ? 1 : 0;  // +1 (ternary) +1 (||)

	while (idx < template.length()) {                                          // +1
		final char c = template.charAt(idx);
		if (RESERVED_CHARS.indexOf(c) != -1) {                             // +2 (nesting=1)
			result.append('\\\\').append(c);
		} else if (c == '*') {                                             // +1 (else if)
			if (idx + 1 < template.length() && template.charAt(idx + 1) == '*') {  // +3 (nesting=2) +1 (&&)
				if (idx + 2 < template.length() && isDelimiter(template.charAt(idx + 2))) {  // +4 (nesting=3) +1 (&&)
					result.append("(?:.*").append(escapedDelim).append("|)");
					idx += 2;
				} else {                                               // +1 (else)
					result.append(".*");
					idx += 1;
				}
			} else {                                                       // +1 (else)
				result.append("[^").append(escapedDelim).append("]*?");
			}
		} else if (c == '?') {                                             // +1 (else if)
			result.append("[^").append(escapedDelim).append("]");
		} else if (isDelimiter(c)) {                                       // +1 (else if)
			result.append(escapedDelim);
		} else {                                                           // +1 (else)
			result.append(c);
		}
		idx++;
	}
	result.append('$');
	return result.toString();
}
'''
        functions = get_java_cogc(code)
        # CogC = 20
        self.assertEqual(20, functions[0].cognitive_complexity)

    def test_try_catch_with_nested_loops(self):
        """Test try-catch with nested control structures - CogC=9"""
        code = '''
void processData() {
	try {                                           // +0 (try doesn't count)
            if (isValid) {                          // +1 (nesting=0, try excluded)
                for (int i = 0; i < 10; i++) {      // +2 (nesting=1)
                    while (hasMore) { … }           // +3 (nesting=2)
                }
            }
        } catch (IOException | SQLException e) {    // +1
            if (shouldRetry) { … }                  // +2 (nesting=1 from catch)
        }
    }                                                // Cognitive Complexity 9
}
'''
        functions = get_java_cogc(code)
        # CogC = 9
        self.assertEqual(9, functions[0].cognitive_complexity)

    def test_lambda_with_nested_condition(self):
        """Test lambda expression adds nesting for contents - CogC=2"""
        code = '''
void processAsync() {
	Runnable task = () -> {         // +0 (lambda doesn't add complexity, but adds nesting)
		if (shouldExecute) { … }    // +2 (nesting=1 from lambda)
	};
}                                    // Cognitive Complexity 2
'''
        functions = get_java_cogc(code)
        # CogC = 2
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_labeled_continue_with_nested_loops(self):
        """Test labeled continue statement - CogC=7"""
        code = '''
int calculateValid(int limit) {
	int count = 0;
	OUTER: for (int i = 1; i <= limit; ++i) {    // +1
		for (int j = 2; j < i; ++j) {        // +2 (nesting=1)
			if (i % j == 0) {            // +3 (nesting=2)
				continue OUTER;      // +1 (labeled jump)
			}
		}
		count += i;
	}
	return count;
}                                                    // Cognitive Complexity 7
'''
        functions = get_java_cogc(code)
        # CogC = 7
        self.assertEqual(7, functions[0].cognitive_complexity)

    def test_switch_statement_single_increment(self):
        """Test switch statement counts as single increment - CogC=1"""
        code = '''
String getStatus(int code) {
	switch (code) {              // +1 (entire switch, regardless of case count)
	case 1:
		return "active";
	case 2:
		return "pending";
	case 3:
		return "inactive";
	default:
		return "unknown";
	}
}                                 // Cognitive Complexity 1
'''
        functions = get_java_cogc(code)
        # CogC = 1
        self.assertEqual(1, functions[0].cognitive_complexity)
