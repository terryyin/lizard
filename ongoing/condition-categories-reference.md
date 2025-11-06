# Condition Categories Reference

## Overview

Lizard separates complexity-contributing constructs into four distinct categories. Each category represents a different type of code construct that adds to cyclomatic complexity.

## The Four Categories

### 1. Control Flow Keywords
**Purpose**: Keywords that create decision points or control structures  
**Variable**: `_control_flow_keywords`

**Common keywords**:
- Conditionals: `if`, `elif`, `elsif`, `else if`, `unless`
- Loops: `for`, `foreach`, `while`, `until`, `repeat`, `do`
- Exception handling: `catch`, `rescue`, `except`, `finally`, `try`
- Pattern matching: `match`, `switch`, `guard`, `when`

**Examples**:
```python
# Python
_control_flow_keywords = {'if', 'elif', 'for', 'while', 'except', 'finally'}

# C#
_control_flow_keywords = {'if', 'for', 'while', 'catch'}

# Fortran (case-insensitive)
_control_flow_keywords = {'IF', 'DO', 'if', 'do'}
```

### 2. Logical Operators
**Purpose**: Operators that combine boolean conditions  
**Variable**: `_logical_operators`

**Symbol-based** (C-style languages):
- `&&` (logical AND)
- `||` (logical OR)

**Word-based** (script languages):
- `and`, `or`, `not`, `xor`

**Special operators**:
- `.AND.`, `.OR.` (Fortran)
- `orelse` (Zig null coalescing)

**Examples**:
```python
# TypeScript
_logical_operators = {'&&', '||'}

# Python
_logical_operators = {'and', 'or'}

# Perl (both forms)
_logical_operators = {'&&', '||'}  # Also has 'and', 'or' with different precedence

# R (both short-circuit and element-wise)
_logical_operators = {'&&', '||', '&', '|'}
```

### 3. Case Keywords
**Purpose**: Keywords for switch/case branch labels  
**Variable**: `_case_keywords`

**Common keywords**:
- `case` (C-style switch)
- `when` (Ruby, Scala case expressions)

**Note**: Many modern languages use pattern matching instead of traditional case:
```python
# Traditional case/switch
_case_keywords = {'case'}

# Pattern matching (Rust, Kotlin)
_case_keywords = set()  # Use match arms or when expressions instead
```

**Examples**:
```python
# C#, TypeScript
_case_keywords = {'case'}

# Rust (uses match arms, not case)
_case_keywords = set()

# Scala (uses case in pattern matching)
_case_keywords = {'case'}
```

### 4. Ternary Operators
**Purpose**: Conditional expression operators  
**Variable**: `_ternary_operators`

**Common operators**:
- `?` (C-style ternary: `condition ? true_val : false_val`)
- `??` (null-coalescing in C#)
- `?:` (Elvis operator in Kotlin)
- `=>` (Zig error unions)

**Special meanings**:
- Python: Uses `x if c else y` syntax, not `?`
- Erlang: `?` is macro expansion, not ternary
- Rust: `?` is error propagation operator

**Examples**:
```python
# C-style languages
_ternary_operators = {'?'}

# C# (both ternary and null-coalescing)
_ternary_operators = {'?', '??'}

# Python (no ternary operator)
_ternary_operators = set()

# Erlang (? is macro, but still counts)
_ternary_operators = {'?'}
```

## How They're Used

### Automatic CCN Calculation
All categories are combined into `reader.conditions`:
```python
# In CodeReader.__init__
self.conditions = (self._control_flow_keywords | 
                  self._logical_operators |
                  self._case_keywords |
                  self._ternary_operators)
```

Each token in `conditions` adds +1 to cyclomatic complexity.

### Extension Access
Extensions can target specific categories:
```python
# lizardnonstrict: Remove only logical operators
reader.conditions -= reader.logical_operators

# Custom extension: Target only control flow
for token in reader.control_flow_keywords: ...
```

## Language-Specific Notes

### R Language
Both short-circuit (`&&`, `||`) and element-wise (`&`, `|`) operators count:
```python
_logical_operators = {'&&', '||', '&', '|'}
```
Rationale: Even vectorized operations represent conditional logic.

### Erlang
`?` is macro expansion, not ternary:
```python
_ternary_operators = {'?'}  # Macro usage adds to complexity
```

### GDScript
Uses Python-like syntax, includes `elif`:
```python
_control_flow_keywords = {'if', 'elif', 'for', 'while', 'catch', 'do'}
```

### ST (Structured Text)
Case-insensitive language with AND/OR:
```python
_control_flow_keywords = {'if', 'elsif', 'IF', 'ELSIF', ...}
_logical_operators = {'and', 'or', 'AND', 'OR'}
```

## Validation Checklist

When implementing a language, verify:

- [ ] All control flow keywords included
- [ ] All logical operators included
- [ ] Case/pattern matching correctly categorized
- [ ] Ternary/conditional operators identified
- [ ] Case-insensitive languages have both cases
- [ ] Empty sets used for unused categories
- [ ] Tests cover all categories
- [ ] Unusual operators are documented

## CCN Calculation Examples

### Example 1: C-style
```c
int func(int x, int y) {
    if (x > 0 && y > 0) {  // +1 (if) +1 (&&)
        switch(x) {         // +0 (switch itself)
            case 1:         // +1
            case 2:         // +1
                break;
        }
    }
    return x > 0 ? 1 : -1;  // +1 (?)
}
// Total CCN: 1 (base) + 1 + 1 + 1 + 1 + 1 = 6
```

### Example 2: Python-style
```python
def func(x, y):
    if x > 0 and y > 0:  # +1 (if) +1 (and)
        for i in range(10):  # +1 (for)
            try:
                process()
            except Exception:  # +1 (except)
                pass
# Total CCN: 1 (base) + 1 + 1 + 1 + 1 = 5
```

## Available Attributes

Each reader instance exposes:
- `reader.conditions` - Combined set of all categories
- `reader.control_flow_keywords` - Control flow only
- `reader.logical_operators` - Logical operators only
- `reader.case_keywords` - Case labels only
- `reader.ternary_operators` - Ternary operators only

These can be used by extensions for fine-grained control.

## Summary

The separated condition categories provide:
- **Clarity**: Each token's purpose is explicit
- **Flexibility**: Extensions can target specific types
- **Correctness**: Easier to verify completeness
- **Maintainability**: Self-documenting code structure

Use this guide when implementing new language support or reviewing existing implementations.

