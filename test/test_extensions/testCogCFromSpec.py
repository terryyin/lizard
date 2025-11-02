'''
Test Cognitive Complexity calculations.
Tests based on the Cognitive Complexity specification.
'''
import unittest
from lizard import analyze_file


def get_cpp_function_list(source_code):
    return analyze_file.analyze_source_code("test.cpp", source_code).function_list


def get_java_function_list(source_code):
    return analyze_file.analyze_source_code("test.java", source_code).function_list


def get_js_function_list(source_code):
    return analyze_file.analyze_source_code("test.js", source_code).function_list


def get_python_function_list(source_code):
    return analyze_file.analyze_source_code("test.py", source_code).function_list

class TestCognitiveComplexitySpecExamples(unittest.TestCase):
    '''
    Test examples from the specification document
    These tests are handpicked from the Cognitive Complexity specification.
    DO NOT CHANGE THE ASSERTED VALUES - FIX THE IMPLEMENTATION INSTEAD.
    These should be treated as canonical examples.
    '''
    def test_Increment_for_nested_flow_break_structures_A(self):
        """Increment for nested flow-break structures 
        TREAT THIS LIKE GOSBEL"""
        code = '''
void myMethod () {
	try {
            if (condition1) { 						// +1
                for (int i = 0; i < 10; i++) { 		// +2 (nesting=1)
                    while (condition2) { … } 		// +3 (nesting=2)
                }
            }
        } catch (ExcepType1 | ExcepType2 e) { 		// +1
            if (condition2) { … } 					// +2 (nesting=1)
        }
    } 												// Cognitive Complexity 9
}
'''
        functions = get_java_function_list(code)
     
        self.assertEqual(9, functions[0].cognitive_complexity)

    def test_Increment_for_nested_flow_break_structures_B(self):
        """Increment for nested flow-break structures 
        TREAT THIS LIKE GOSBEL"""
        code = '''
void myMethod2 () {
	Runnable r = () -> { 			// +0 (but nesting level is now 1)
		if (condition1) { … } 		// +2 (nesting=1)
	};
}                                   // Cognitive Complexity 2
'''
        functions = get_java_function_list(code)
     
        self.assertEqual(2, functions[0].cognitive_complexity)
    def test_Increment_for_nested_flow_break_structures_C(self):
        """Appendix C: Examples From com.persistit.TimelyResource.java in sonar-persistit
        TREAT THIS LIKE GOSBEL"""
        code = '''
#if DEBUG                           // +1 for if
void myMethod2 () { 				// +0 (nesting level is still 0)
	Runnable r = () -> { 			// +0 (but nesting level is now 1)
		if (condition1) { … } 		// +3 (nesting=2)
		};
} 									// Cognitive Complexity 4
#endif
'''
        functions = get_java_function_list(code)
        # SPEC REQUIREMENT: CogC = 4 (preprocessor #if adds +1 to nesting, so lambda if is +3)
        # DO NOT CHANGE THIS VALUE - FIX THE IMPLEMENTATION INSTEAD
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_Intuitively_right_complexity_scores_A(self):
        """TREAT THIS LIKE GOSBEL"""
        code = '''
int sumOfPrimes(int max) {
	int total = 0;
	OUT: for (int i = 1; i <= max; ++i) { 	    // +1
		for (int j = 2; j < i; ++j) { 			// +2
			if (i % j == 0) { 					// +3
				continue OUT; 					// +1
			}
		}
		total += i;
	}
	return total;
} 												// Cognitive Complexity 7
'''
        functions = get_java_function_list(code)
     
        self.assertEqual(7, functions[0].cognitive_complexity)
    def test_Intuitively_right_complexity_scores_B(self):
        """TREAT THIS LIKE GOSBEL"""
        code = '''
String getWords(int number) {
	switch (number) { 			// +1
	case 1:
		return “one”;
	case 2:
		return “a couple”;
	case 3:
		return “a few”;
	default:
		return “lots”;
	}
} 								// Cognitive Complexity 1
'''
        functions = get_java_function_list(code)
        # SPEC REQUIREMENT: CogC = 1 for switch statement (counts as single decision point)
        # DO NOT CHANGE THIS VALUE - FIX THE IMPLEMENTATION INSTEAD
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_JavaScript_Missing_class_structures_A(self):
        """Appendix A: Decorators TREAT THIS LIKE GOSBEL"""
        code = '''
function(...) { 					// declarative; ignored
	var foo;

	bar.myFun = function(…) { 		// nesting = 0
		if(condition) {			 	// +1
			...
		}
	}
} 								// total complexity = 1
'''
        functions = get_js_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_JavaScript_Missing_class_structures_B(self):
        """Appendix A: Decorators TREAT THIS LIKE GOSBEL"""
        code = '''
function(...) { 					// non-declarative; not ignored
	var foo;
    if (condition) { 				// +1; top-level structural increment
        ...
	}

	bar.myFun = function(…) { 		// nesting = 1
		if(condition) {             // +2
		    ...
		}
	}
} 									// total complexity = 3
'''
        functions = get_js_function_list(code)
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_Decorators_A(self):
        """Appendix A: Decorators TREAT THIS LIKE GOSBEL"""
        code = '''
def a_decorator(a, b):
	def inner(func): 				# nesting = 0
		if condition: 				# +1
			print(b)
		func()
	return inner 					# total = 1
'''
        functions = get_python_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_Decorators_B(self):
        """Appendix A: Decorators
        TREAT THIS LIKE GOSBEL"""
        code = '''
def not_a_decorator(a, b):
	my_var = a*b
	def inner(func):				# nesting = 1
		if condition: 				# +1 structure, +1 nesting
			print(b)
		func()
	return inner 					# total = 2
'''
        functions = get_python_function_list(code)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_Decorators_C(self):
        """Appendix A: Decorators
         TREAT THIS LIKE GOSBEL"""
        code = '''
def decorator_generator(a):
	def generator(func):
		def decorator(func):	# nesting = 0
			if condition: 			# +1
				print(b)
			return func()
		return decorator
	return generator 			# total = 1
'''
        functions = get_python_function_list(code)

        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_model_js(self):
        """Appendix C: Examples From model.js in YUI
        TREAT THIS LIKE GOSBEL"""
        code = '''
var model = {
save: function (options, callback) {
	var self = this;
	
	if (typeof options === 'function') { // +1
		callback = options;
		options = {};
	}
	
	options || (options = {}); // +1
	
	self._validate(self.toJSON(), function (err) {
		if (err) { // +2 (nesting = 1)
			callback && callback.call(null, err); // +1
			return;
		}
		
		self.sync(self.isNew() ? 'create' : 'update', // +2 (nesting = 1)
			options, function (err, response) {
			var facade = {
					options : options,
					response: response
				},
			parsed;
			
			if (err) { // +3 (nesting = 2)
				facade.error = err;
				facade.src = 'save';
				self.fire(EVT_ERROR, facade);
				
			} else { // +1
				if (!self._saveEvent) { // +4 (nesting = 3)
					self._saveEvent = self.publish(EVT_SAVE, {
						preventable: false
					});
				}
				if (response) { // +4 (nesting = 3)
					parsed = facade.parsed = self._parse(response);
					self.setAttrs(parsed, options);
				}
				
				self.changed = {};
				self.fire(EVT_SAVE, facade);
			}
			
			callback && callback.apply(null, arguments); // +1
		});
	});
	return self;
} // total complexity = 20
};
'''
        functions = get_js_function_list(code)
        # SPEC REQUIREMENT: CogC = 20 from Appendix C example
        # DO NOT CHANGE THIS VALUE - FIX THE IMPLEMENTATION INSTEAD
        self.assertEqual(20, functions[0].cognitive_complexity)

    def test_org_sonar_java_resolve_JavaSymbol_java(self):
        """Appendix C: Examples From org.sonar.java.resolve.JavaSymbol.java in the SonarJava analyzer TREAT THIS LIKE GOSBEL"""
        code = '''
@Nullable
private MethodJavaSymbol overriddenSymbolFrom(ClassJavaType classType) {
	if (classType.isUnknown()) {
		return Symbols.unknownMethodSymbol;
	}

	boolean unknownFound = false;
	List<JavaSymbol> symbols = classType.getSymbol().members().lookup(name);
	for (JavaSymbol overrideSymbol : symbols) {
		if (overrideSymbol.isKind(JavaSymbol.MTH)
			    && !overrideSymbol.isStatic()) {
		MethodJavaSymbol methodJavaSymbol = (MethodJavaSymbol)overrideSymbol;
		if (canOverride(methodJavaSymbol)) {
			Boolean overriding = checkOverridingParameters(methodJavaSymbol, 
                            classType);
				if (overriding == null) {
					if (!unknownFound) {
						unknownFound = true;
					}
				} else if (overriding) {
					return methodJavaSymbol;
				}
			}
		}
	}

	if (unknownFound) {
		return Symbols.unknownMethodSymbol;
	}
	return null;
}
'''
        functions = get_java_function_list(code)
     
        self.assertEqual(19, functions[0].cognitive_complexity)

    def test_com_persistit_TimelyResource_java(self):
        """Appendix C: Examples FFrom com.persistit.TimelyResource.java in sonar-persistit TREAT THIS LIKE GOSBEL"""
        code = '''
private void addVersion(final Entry entry, final Transaction txn)
		throws PersistitInterruptedException, RollbackException {
	final TransactionIndex ti = _persistit.getTransactionIndex();
	while (true) {
		try {
			synchronized (this) {
				if (frst != null) {
					if (frst.getVersion() > entry.getVersion()) {
						throw new RollbackException();
					}
					if (txn.isActive()) {
						for
                                (Entry e = frst; e != null; e = e.getPrevious()) {
                            final long version = e.getVersion();
                            final long depends = ti.wwDependency(version,
                                txn.getTransactionStatus(), 0);
                            if (depends == TIMED_OUT) {
                                throw new WWRetryException(version);
                            }
                            if (depends != 0
                                    && depends != ABORTED) {
                                throw new RollbackException();
                            }
					    }
				    }
                }
                entry.setPrevious(frst);
                frst = entry;
                break;
            }
        } catch (final WWRetryException re) {
            try {
                final long depends = _persistit.getTransactionIndex()
                        .wwDependency(re.getVersionHandle(),txn.getTransactionStatus(),
                        SharedResource.DEFAULT_MAX_WAIT_TIME);
                if (depends != 0
                        && depends != ABORTED) {
                    throw new RollbackException();
                }
            } catch (final InterruptedException ie) {
                throw new PersistitInterruptedException(ie);				
            }
        } catch (final InterruptedException ie) {
            throw new PersistitInterruptedException(ie);
        }
	}
}
'''
        functions = get_java_function_list(code)
     
        self.assertEqual(35, functions[0].cognitive_complexity)

    def test_org_sonar_api_utils_WildcardPattern_java(self):
        """Appendix C: Examples From org.sonar.api.utils.WildcardPattern.java in SonarQube
        TREAT THIS LIKE GOSBEL"""
        code = '''
private static String toRegexp(String antPattern, String directorySeparator) {
	final String escapedDirectorySeparator = "\\\\" + directorySeparator;
	final StringBuilder sb = new StringBuilder(antPattern.length());
	sb.append('^');
	int i = antPattern.startsWith("/") || antPattern.startsWith("\\\\") ? 1 : 0;

	while (i < antPattern.length()) {
		final char ch = antPattern.charAt(i);
		if (SPECIAL_CHARS.indexOf(ch) != -1) {
			sb.append('\\\\').append(ch);
		} else if (ch == '*') {
			if (i + 1 < antPattern.length() && antPattern.charAt(i + 1) == '*') {
				if (i + 2 < antPattern.length() && isSlash(antPattern.charAt(i + 2))) {
					sb.append("(?:.*").append(escapedDirectorySeparator).append("|)");
					i += 2;
				} else {
					sb.append(".*");
					i += 1;
				}
			} else {
				sb.append("[^").append(escapedDirectorySeparator).append("]*?");
			}
		} else if (ch == '?') {
			sb.append("[^").append(escapedDirectorySeparator).append("]");
		} else if (isSlash(ch)) {
			sb.append(escapedDirectorySeparator);
		} else {
			sb.append(ch);
		}
		i++;
	}
	sb.append('$');
	return sb.toString();
}
'''
        functions = get_java_function_list(code)
        # SPEC REQUIREMENT: Cognitive Complexity = 20 from Appendix C
        # DO NOT CHANGE THIS VALUE - FIX THE IMPLEMENTATION INSTEAD
        self.assertEqual(20, functions[0].cognitive_complexity)


if __name__ == '__main__':
    unittest.main()