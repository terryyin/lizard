# Fix for Issue #451: IndexError with C++ Raw String Literals

## Problem Description

Lizard crashed with an `IndexError: list assignment index out of range` when parsing C++ raw string literals containing braces, such as:

```cpp
const char* json = R"({
  "name": "Ada Lovelace",
  "profile": {
    "bio": "Mathematician & pioneer"
  }
})";
```

The error occurred in `lizard_ext/lizardns.py` at line 104:
```python
self.structure_piles[-1] = 0
IndexError: list assignment index out of range
```

## Root Cause

C++ raw string literals (format: `R"delimiter(content)delimiter"`) were not being tokenized correctly. The tokenizer was splitting them into multiple tokens:
- `R` (as an identifier)
- `"({"` (as a string)
- `name` (as an identifier)
- etc.

This caused the braces inside the raw string to be treated as actual code braces, leading to unbalanced brace counting in the nested structures extension (lizardns). When the extension tried to pop more items than existed in the `structure_piles` list, it resulted in an IndexError.

## Solution

Added support for C++ raw string literals to the tokenizer in `lizard_languages/clike.py`:

1. **Added regex pattern** to properly match raw string literals:
   ```python
   r"|R\"[^(\\]*\((?:[^)]|\)[^\"])*\)[^(\\]*\""
   ```

   This pattern handles:
   - Simple raw strings: `R"(content)"`
   - Raw strings with delimiters: `R"delim(content)delim"`
   - Raw strings containing braces, quotes, and other special characters

2. **Added test cases** in `test/testNestedStructures.py`:
   - `test_raw_string_literal_with_braces`: Tests the exact scenario from the issue
   - `test_raw_string_literal_with_delimiter`: Tests raw strings with custom delimiters
   - `test_raw_string_literal_simple`: Tests simple raw strings

## Verification

1. All existing tests pass (754+ tests)
2. New test cases specifically for raw strings pass
3. The exact example from issue #451 now runs successfully:
   ```bash
   lizard --csv -ENS main.cpp
   # Output: 22,3,78,0,27,"main@3-29@main.cpp","main.cpp","main","main()",3,29,2
   ```

## Files Changed

- `lizard_languages/clike.py`: Added raw string literal pattern to token generation
- `test/testNestedStructures.py`: Added three new test cases for raw string literals

## Impact

This fix ensures that C++ raw string literals are correctly treated as single tokens, preventing any characters inside them (including braces) from being misinterpreted as code structure elements.
