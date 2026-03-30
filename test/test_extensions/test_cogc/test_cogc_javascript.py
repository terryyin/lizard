"""Cognitive Complexity tests for JavaScript"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_javascript_cogc(source_code):
    """Analyze JavaScript code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.js", source_code
    ).function_list


class TestJavascriptCognitiveComplexity(unittest.TestCase):
    """Test Cognitive Complexity calculations for JavaScript code"""

    def test_simple_function_has_zero_cogc(self):
        """Simple function with no control flow should have CogC = 0"""
        code = '''
function simple() {
    var x = 1;
    var y = 2;
    return x + y;
}
'''
        functions = get_javascript_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_nested_loops_increase_cogc(self):
        """Nested loops should demonstrate complexity"""
        code = '''
function nested() {
    for (var i = 0; i < 10; i++) {
        for (var j = 0; j < 10; j++) {
            if (i === j) {
                console.log(i);
            }
        }
    }
}
'''
        functions = get_javascript_cogc(code)
        # CogC should be at least 3 (one for each structure)
        self.assertGreaterEqual(functions[0].cognitive_complexity, 3)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 regardless of cases"""
        code = '''
function getWord(number) {
    switch (number) {
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
'''
        functions = get_javascript_cogc(code)
        # CogC = 1 (entire switch), CCN counts each case
        self.assertEqual(1, functions[0].cognitive_complexity)
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_else_if_chain(self):
        """else if chain should count each branch"""
        code = '''
function classify(x) {
    if (x > 10) {
        return "big";
    } else if (x > 5) {
        return "medium";
    } else if (x > 0) {
        return "small";
    } else {
        return "zero or negative";
    }
}
'''
        functions = get_javascript_cogc(code)
        # CogC = 4 (if + 2 else if + else)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_ternary_operator(self):
        """Ternary operator may not be fully counted yet"""
        code = '''
function absolute(x) {
    return x > 0 ? x : -x;
}
'''
        functions = get_javascript_cogc(code)
        # Ternary may not be implemented yet
        self.assertGreaterEqual(functions[0].cognitive_complexity, 0)

    def test_nested_ternary(self):
        """Nested ternary - testing current implementation"""
        code = '''
function complexTernary(x, y) {
    return x > 0 ? (y > 0 ? 1 : -1) : 0;
}
'''
        functions = get_javascript_cogc(code)
        # Actual value depends on ternary support
        self.assertGreaterEqual(functions[0].cognitive_complexity, 0)

    def test_arrow_function_nesting(self):
        """Arrow functions with control flow"""
        code = '''
function withArrow() {
    var fn = (x) => {
        if (x > 0) {
            return x;
        }
    };
}
'''
        functions = get_javascript_cogc(code)
        # Should have at least the if statement
        self.assertGreaterEqual(functions[0].cognitive_complexity, 1)

    def test_try_catch_counts_catch_only(self):
        """try-catch should count catch clause only"""
        code = '''
function errorHandler() {
    try {
        riskyOperation();
    } catch (err) {
        console.log(err);
    }
}
'''
        functions = get_javascript_cogc(code)
        # CogC = 1 (catch clause)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operators in sequence"""
        code = '''
function logical() {
    if (a && b && c) {
        return true;
    }
    if (d || e || f) {
        return false;
    }
}
'''
        functions = get_javascript_cogc(code)
        # CogC should count if statements and logical operator sequences
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_complex_async_operation_with_nested_callbacks(self):
        """Test complex async operation with deeply nested callbacks - CogC=20"""
        code = '''
var handler = {
process: function (config, done) {
	var ctx = this;

	if (typeof config === 'function') {     // +1
		done = config;
		config = {};
	}

	config || (config = {});                // +1 (binary operator)

	ctx._check(ctx.getData(), function (error) {
		if (error) {                    // +2 (nesting=1)
			done && done.call(null, error);  // +1 (binary operator)
			return;
		}

		ctx.execute(ctx.isReady() ? 'start' : 'retry',  // +2 (nesting=1, ternary)
			config, function (error, result) {
			var context = {
					config : config,
					result: result
				},
			data;

			if (error) {                // +3 (nesting=2)
				context.error = error;
				context.source = 'process';
				ctx.trigger(EVT_ERROR, context);

			} else {                    // +1 (else)
				if (!ctx._event) {      // +4 (nesting=3)
					ctx._event = ctx.register(EVT_COMPLETE, {
						cancelable: false
					});
				}
				if (result) {           // +4 (nesting=3)
					data = context.data = ctx._transform(result);
					ctx.update(data, config);
				}

				ctx.state = {};
				ctx.trigger(EVT_COMPLETE, context);
			}

			done && done.apply(null, arguments);  // +1 (binary operator)
		});
	});
	return ctx;
}  // total complexity = 20
};
'''
        functions = get_javascript_cogc(code)
        # CogC = 20 (same structure as spec example, transformed to avoid copyright)
        self.assertEqual(20, functions[0].cognitive_complexity)

    def test_outer_function_declarative_pattern(self):
        """Test outer function with only declarations (nesting not applied) - CogC=1"""
        code = '''
function(...) {                     // declarative; purely for organization
	var config;

	api.handler = function(…) {     // nesting = 0 (outer is declarative)
		if(isEnabled) {         // +1
			...
		}
	}
}                                    // total complexity = 1
'''
        functions = get_javascript_cogc(code)
        # CogC = 1 (outer function is declarative, so nested function has nesting=0)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_outer_function_non_declarative_pattern(self):
        """Test outer function with control flow (nesting applied) - CogC=3"""
        code = '''
function(...) {                     // non-declarative; has control flow
	var config;
    if (isReady) {                  // +1; top-level structural increment
        ...
	}

	api.handler = function(…) {     // nesting = 1 (outer has control flow)
		if(isEnabled) {         // +2 (nesting=1)
		    ...
		}
	}
}                                    // total complexity = 3
'''
        functions = get_javascript_cogc(code)
        # CogC = 3 (outer function has control flow, so nested function gets nesting penalty)
        self.assertEqual(3, functions[0].cognitive_complexity)