# Lizard Code Structure Reference

## Condition Handling System

### Base Class: CodeReader

**Location**: `lizard_languages/code_reader.py`

The `CodeReader` class defines four condition category fields:

```python
class CodeReader:
    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    
    @classmethod
    def _build_conditions(cls):
        """Combines all categories into one set."""
        return (cls._control_flow_keywords | 
                cls._logical_operators | 
                cls._case_keywords | 
                cls._ternary_operators)
    
    def __init__(self, context):
        # Combined set for compatibility
        self.conditions = copy(self.__class__._build_conditions())
        
        # Individual sets for extensions
        self.control_flow_keywords = copy(self.__class__._control_flow_keywords)
        self.logical_operators = copy(self.__class__._logical_operators)
        self.case_keywords = copy(self.__class__._case_keywords)
        self.ternary_operators = copy(self.__class__._ternary_operators)
```

### Language Reader Pattern

Each language overrides the category fields:

```python
class LanguageReader(CodeReader):
    _control_flow_keywords = {'if', 'for', 'while'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
```

### How CCN is Calculated

**Location**: `lizard.py:condition_counter()`

```python
def condition_counter(tokens, reader):
    conditions = reader.conditions  # Combined set of all categories
    for token in tokens:
        if token in conditions:
            reader.context.add_condition()  # Adds +1 to CCN
        yield token
```

Each token in `reader.conditions` adds +1 to the function's cyclomatic complexity.

## Extension Patterns

### Pattern 1: Remove Specific Category

**Example**: `lizardnonstrict.py` removes logical operators

```python
class LizardExtension(object):
    def __call__(self, tokens, reader):
        # Remove logical operators from conditions
        reader.conditions -= reader.logical_operators
        return tokens
```

**Effect**: Excludes logical operators from CCN calculation.

### Pattern 2: Special Handling for Category

**Example**: `lizardmccabe.py` handles consecutive case statements

```python
class LizardExtension(ExtensionBase):
    def _state_global(self, token):
        if token == "case":  # Detect case keywords
            self._state = self._in_case
    
    def _after_a_case(self, token):
        if token == "case":  # Consecutive case
            self.context.add_condition(-1)  # Subtract complexity
```

**Effect**: Only first case in sequence adds to CCN.

### Pattern 3: Track All Categories

**Example**: `lizardcomplextags.py` records all complexity keywords

```python
class LizardExtension(object):
    def __call__(self, tokens, reader):
        conditions = reader.conditions  # Use combined set
        for token in tokens:
            if token in conditions:
                # Record [token, line_number]
                context.current_function.complex_tags.append([token, line])
            yield token
```

**Effect**: Logs all complexity-contributing tokens.

## Language-Specific Implementations

### Python
```python
_control_flow_keywords = {'if', 'elif', 'for', 'while', 'except', 'finally'}
_logical_operators = {'and', 'or'}
_case_keywords = set()  # No case in Python
_ternary_operators = set()  # Uses 'x if c else y' syntax
```

### TypeScript
```python
_control_flow_keywords = {'if', 'elseif', 'for', 'while', 'catch'}
_logical_operators = {'&&', '||'}
_case_keywords = {'case'}
_ternary_operators = {'?'}
```

### R
```python
_control_flow_keywords = {'if', 'else if', 'for', 'while', 'repeat', 'switch', 'tryCatch', 'try', 'ifelse'}
_logical_operators = {'&&', '||', '&', '|'}  # Both short-circuit and element-wise
_case_keywords = set()
_ternary_operators = set()
```

### Rust
```python
_control_flow_keywords = {'if', 'for', 'while', 'catch', 'match', 'where'}
_logical_operators = {'&&', '||'}
_case_keywords = set()  # Rust uses match arms, not case
_ternary_operators = {'?'}  # Error propagation operator
```

### Fortran (Case-insensitive)
```python
_control_flow_keywords = {'IF', 'DO', 'if', 'do'}
_logical_operators = {'.AND.', '.OR.', '.and.', '.or.'}
_case_keywords = {'CASE', 'case'}
_ternary_operators = set()
```

## Special Cases

### Languages with Multiple Operator Forms

**Perl** - Both word and symbol forms:
```python
_logical_operators = {'&&', '||'}  # Symbol form
# Note: Also has 'and', 'or' with different precedence (not included)
```

**PHP** - Symbol primary, word exists:
```python
_logical_operators = {'&&', '||'}
# Note: Also has 'and', 'or' with different precedence
```

### Languages with Unusual Operators

**Erlang** - `?` is macro expansion:
```python
_ternary_operators = {'?'}  # Macro operator, not ternary
```

**Zig** - Multiple special operators:
```python
_logical_operators = {'and', 'or', 'orelse'}  # orelse = null coalescing
_ternary_operators = {'=>'}  # Error union and switch cases
```

**Kotlin** - Elvis operator:
```python
_ternary_operators = {'?:'}  # Elvis operator
```

## Instance Attributes Available

After initialization, each reader instance has:

```python
reader.conditions               # Set: Combined all categories
reader.control_flow_keywords    # Set: Control flow only
reader.logical_operators        # Set: Logical operators only
reader.case_keywords           # Set: Case keywords only
reader.ternary_operators       # Set: Ternary operators only
```

All are mutable sets that extensions can modify.

## Backward Compatibility

The base class supports two approaches for defining conditions:

```python
# Approach 1: Define _conditions directly (for compatibility)
class CustomReader(CodeReader):
    _conditions = {'if', 'for', '&&', '||', 'case', '?'}  # Combined set

# Approach 2: Use separated categories (recommended)
class ModernReader(CodeReader):
    _control_flow_keywords = {'if', 'for'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
```

Both approaches work identically. Separated categories provide better semantics and enable extensions to target specific types.

## Quick Reference Table

| Category | Purpose | Example Tokens | Use Case |
|----------|---------|----------------|----------|
| `_control_flow_keywords` | Decision points | if, for, while, catch | Core control structures |
| `_logical_operators` | Compound conditions | &&, \|\|, and, or | Combining conditions |
| `_case_keywords` | Switch branches | case, when | Multi-way branches |
| `_ternary_operators` | Inline conditions | ?, ??, ?: | Conditional expressions |

## Summary

The separated condition categories provide:
- **Semantic clarity**: Each token's role is explicit
- **Extension flexibility**: Target specific types
- **Easier validation**: Check each category independently
- **Better maintenance**: Self-documenting structure

All categories combine into `reader.conditions` for CCN calculation, maintaining compatibility while providing better organization.

