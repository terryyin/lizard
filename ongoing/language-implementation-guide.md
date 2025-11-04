# Language Implementation Guide for Lizard

## Overview

This guide explains how to implement support for a new programming language in Lizard code analyzer.

## Language Reader Structure

Each language implementation consists of:
- A reader class inheriting from `CodeReader`
- Token generation logic
- Language-specific state machine
- Condition categorization

## Step 1: Create Language Reader Class

Create a new file in `lizard_languages/` named after your language (e.g., `mylang.py`):

```python
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin  # If language has C-style comments

class MyLanguageReader(CodeReader, CCppCommentsMixin):
    ext = ['mylang']  # File extensions
    language_names = ['mylanguage', 'mylang']  # Command line names
    
    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    
    def __init__(self, context):
        super(MyLanguageReader, self).__init__(context)
        self.parallel_states = [MyLanguageStates(context)]
```

## Step 2: Define Condition Categories

Categorize language keywords into four distinct types:

### 1. Control Flow Keywords (`_control_flow_keywords`)
Keywords that create decision points or control structures:
- `if`, `elif`, `else if`, `unless`
- `for`, `foreach`, `while`, `until`, `repeat`
- `catch`, `rescue`, `except`, `finally`
- `switch`, `match`, `guard`

**Rule**: Keywords that create new execution paths.

### 2. Logical Operators (`_logical_operators`)
Operators that combine boolean conditions:
- Symbol-based: `&&`, `||`
- Word-based: `and`, `or`, `not`, `xor`
- Special: `orelse` (Zig), `.AND.` (Fortran)

**Rule**: Operators that add decision points within conditions.

### 3. Case Keywords (`_case_keywords`)
Keywords for switch/case branches:
- `case`, `when` (for case expressions)

**Rule**: Keywords for individual switch branches (not the switch itself).

### 4. Ternary Operators (`_ternary_operators`)
Conditional expression operators:
- `?` (C-style ternary)
- `??` (null-coalescing)
- `?:` (Elvis operator)

**Rule**: Operators for inline conditional expressions.

## Step 3: Implement Token Generation

```python
@staticmethod
def generate_tokens(source_code, addition='', token_class=None):
    # Add language-specific patterns
    addition = addition + r"|pattern1|pattern2"
    return CodeReader.generate_tokens(source_code, addition, token_class)
```

## Step 4: Create State Machine

```python
class MyLanguageStates(CodeStateMachine):
    def _state_global(self, token):
        if token == 'func':
            self.context.push_new_function('anonymous')
            self._state = self._function_name
    
    def _function_name(self, token):
        self.context.current_function.name = token
        self._state = self._state_global
```

## Examples by Language Type

### C-like Languages (Symbol operators)
```python
class CStyleReader(CodeReader, CCppCommentsMixin):
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
```

**Examples**: C#, Java, Kotlin, Rust, Scala, Swift, TypeScript

### Script Languages (Word operators)
```python
class ScriptStyleReader(CodeReader):
    _control_flow_keywords = {'if', 'elif', 'for', 'while', 'except'}
    _logical_operators = {'and', 'or'}
    _case_keywords = set()
    _ternary_operators = set()
```

**Examples**: Python, Ruby, Perl

### Mixed Style (Both)
```python
class MixedReader(CodeReader):
    _control_flow_keywords = {'if', 'for', 'while'}
    _logical_operators = {'and', 'or', '&&', '||'}  # Both forms
    _case_keywords = set()
    _ternary_operators = {'?'}
```

**Examples**: Perl, PHP, Ruby

### Case-Insensitive Languages
```python
class CaseInsensitiveReader(CodeReader):
    _control_flow_keywords = {'IF', 'if', 'FOR', 'for'}  # Both cases
    _logical_operators = {'.AND.', '.and.', '.OR.', '.or.'}
    _case_keywords = {'CASE', 'case'}
    _ternary_operators = set()
```

**Examples**: Fortran, PL/SQL, ST

## Step 5: Add Tests

Create test file in `test/test_languages/testMyLang.py`:

```python
import unittest
from lizard import analyze_file

def get_mylang_function_list(source_code):
    return analyze_file.analyze_source_code("test.mylang", source_code).function_list

class TestMyLanguage(unittest.TestCase):
    def test_simple_function(self):
        code = '''
        func simple() {
            return 1
        }
        '''
        result = get_mylang_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("simple", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
    
    def test_complexity(self):
        code = '''
        func complex(x) {
            if x > 0 && y > 0 {
                return 1
            }
        }
        '''
        result = get_mylang_function_list(code)
        self.assertEqual(3, result[0].cyclomatic_complexity)  # base + if + &&
```

## Common Patterns

### Pattern 1: Empty Sets
Use `set()` for categories your language doesn't have:
```python
_case_keywords = set()  # Python uses if/elif, not case
_ternary_operators = set()  # Python uses 'x if c else y', not ?
```

### Pattern 2: Language-Specific Operators
Document unusual operators:
```python
_logical_operators = {'and', 'or', 'orelse'}  # orelse is Zig's null coalescing
_ternary_operators = {'?:'}  # Elvis operator in Kotlin
```

### Pattern 3: Multi-word Keywords
Include complete keywords:
```python
_control_flow_keywords = {'if', 'else if', 'for'}  # 'else if' is one keyword in R
```

## Special Cases

### Vectorized Operations (R)
R has both short-circuit and element-wise operators:
```python
_logical_operators = {'&&', '||', '&', '|'}  # Both types count
```

### Macro Operators (Erlang)
Erlang's `?` is for macro expansion, not ternary:
```python
_ternary_operators = {'?'}  # Macro expansion adds to complexity
```

### Pattern Matching
Languages with pattern matching may not use traditional case:
```python
_control_flow_keywords = {'match', 'when'}  # Rust, Scala
_case_keywords = set()  # Not using traditional case
```

## Integration

1. Add your reader to `lizard_languages/__init__.py`
2. Run tests: `nix develop -c python -m pytest test/test_languages/testMyLang.py`
3. Run all tests: `nix develop -c python -m pytest`

## What Gets Counted

Each token in these categories adds +1 to cyclomatic complexity:
- Each control flow keyword
- Each logical operator
- Each case label
- Each ternary operator

**Base CCN**: Functions start at CCN=1

**Example Calculation**:
```python
def example(x, y):
    if x > 0 and y > 0 or x < -10:  # +1 (if) +1 (and) +1 (or)
        for i in range(10):          # +1 (for)
            pass
# Total CCN: 1 (base) + 3 (if, and, or) + 1 (for) = 5
```

## Best Practices

### DO:
- ✅ Categorize all condition-related keywords
- ✅ Document unusual operators
- ✅ Include all keyword variants (case-insensitive languages)
- ✅ Test with real code samples
- ✅ Use empty sets for unused categories

### DON'T:
- ❌ Mix different concept types together
- ❌ Include 'else' (it's part of if-else, not separate)
- ❌ Guess at categorization - verify with language spec
- ❌ Forget to test edge cases

## Reference Implementations

Good examples to study:
- **Python** (`python.py`) - Clean word-based operators
- **TypeScript** (`typescript.py`) - Clean symbol-based operators
- **Fortran** (`fortran.py`) - Case-insensitive handling
- **R** (`r.py`) - Multiple operator types
- **Rust** (`rust.py`) - Pattern matching instead of case

## How Extensions Use Categories

Extensions can now target specific types:

```python
# Remove only logical operators (non-strict mode)
reader.conditions -= reader.logical_operators

# Access specific categories
if token in reader.control_flow_keywords: ...
if token in reader.case_keywords: ...
```

## Testing Checklist

- [ ] Basic function parsing
- [ ] Control flow keywords (if, for, while)
- [ ] Logical operators (and/or, &&/||)
- [ ] Case/switch statements (if applicable)
- [ ] Ternary operators (if applicable)
- [ ] Nested structures
- [ ] Comments
- [ ] String literals
- [ ] Complex real-world code

## Summary

Implementing a new language requires:
1. Categorizing condition-related keywords into 4 types
2. Implementing token generation
3. Creating state machine for parsing
4. Writing comprehensive tests

The separated categories make it clear what each keyword represents and how it affects complexity calculation.

