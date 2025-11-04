# Language Conditions Inventory

This document lists all `_conditions` definitions across language readers and categorizes them.

## Legend
- 🔀 Control Flow Keywords
- 🔗 Logical Operators  
- 🔢 Case/Switch Keywords
- ❓ Ternary Operators
- ⚠️ Special/Ambiguous

## Complete Inventory

### Base Class (code_reader.py)
```python
_conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}
```
- 🔀 if, for, while, catch
- 🔗 &&, ||
- 🔢 case
- ❓ ?

---

### C# (csharp.py)
```python
_conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch', 'case', '??'])
```
- 🔀 if, for, while, catch
- 🔗 &&, ||
- 🔢 case
- ❓ ?, ??

---

### Erlang (erlang.py)
```python
_conditions = {'and', 'case', 'catch', 'if', 'not', 'or', '?', 'when'}
```
- 🔀 if, catch, when
- 🔗 and, or, not
- 🔢 case, when (dual purpose)
- ❓ ? (different meaning in Erlang)

**Notes:** 
- `when` is used in pattern matching guards
- `?` is a macro operator in Erlang, not ternary

---

### Fortran (fortran.py)
```python
_conditions = {
    'IF', 'DO', '.AND.', '.OR.', 'CASE',
    'if', 'do', '.and.', '.or.', 'case'
}
```
- 🔀 IF/if, DO/do
- 🔗 .AND./.and., .OR./.or.
- 🔢 CASE/case

**Notes:** Case-insensitive language, both cases included

---

### GDScript (gdscript.py)
```python
_conditions = set(['if', 'else', 'for', 'while', '&&', '||', '?', 'catch', 'case', 'do'])
```
- 🔀 if, else, for, while, catch, case, do
- 🔗 &&, ||
- ❓ ?

**Notes:** `else` is unusual to include here

---

### Kotlin (kotlin.py)
```python
_conditions = {'if', 'for', 'while', 'catch', '&&', '||', '?:'}
```
- 🔀 if, for, while, catch
- 🔗 &&, ||
- ❓ ?: (Elvis operator)

---

### Perl (perl.py) - Two definitions!
```python
# Definition 1 (line 22):
_conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach',
                   'or', 'and', '||', '&&', '?', '=~'])

# Definition 2 (line 68):
_conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach',
                   'or', 'and', '||', '&&', '?'])
```
- 🔀 if, elsif, unless, while, until, for, foreach
- 🔗 or, and, ||, &&
- ❓ ?
- ⚠️ =~ (regex match operator in def 1)

**Notes:** Both word and symbol logical operators

---

### PHP (php.py)
```python
_conditions = set(['if', 'elseif', 'for', 'foreach', 'while', '&&', '||', '?',
                   'catch', 'case', 'match'])
```
- 🔀 if, elseif, for, foreach, while, catch, match
- 🔗 &&, ||
- 🔢 case
- ❓ ?

**Notes:** Has special handling for `match` expression

---

### PL/SQL (plsql.py)
```python
_conditions = {"if", "elsif", "when", "while", "for", "and", "or"}
```
- 🔀 if, elsif, when, while, for
- 🔗 and, or

**Notes:** Case-insensitive, uppercased versions added in `__init__`

---

### Python (python.py)
```python
_conditions = set([
    'if', 'for', 'while', 'and', 'or',
    'elif', 'except', 'finally'
])
```
- 🔀 if, for, while, elif, except, finally
- 🔗 and, or

**Notes:** No ternary `?`, Python uses `x if c else y`

---

### R (r.py)
```python
_conditions = {
    'if', 'else if', 'for', 'while', 'repeat', 'switch',
    '&&', '||', '&', '|', 'ifelse',
    'tryCatch', 'try'
}
```
- 🔀 if, else if, for, while, repeat, switch, tryCatch, try
- 🔗 &&, ||, &, |
- ⚠️ ifelse (vectorized if function)

**Notes:** 
- Has both short-circuit (`&&`, `||`) and element-wise (`&`, `|`) operators
- `ifelse` is a vectorized function, questionable if it should add CCN
- ⚠️ **Potential bug**: element-wise `&`, `|` might not be control flow

---

### Ruby/Lua (rubylike.py)
```python
_conditions = set(['if', 'until', 'for', 'while', 'and', 'or',
                   'elsif', 'elseif', 'rescue',
                   'ensure', 'when', '||', '&&', '?'])
```
- 🔀 if, until, for, while, elsif, elseif, rescue, ensure, when
- 🔗 and, or, ||, &&
- ❓ ?

**Notes:** Both word and symbol logical operators

---

### Rust (rust.py)
```python
_conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                   'case', 'match', 'where'])
```
- 🔀 if, for, while, catch, match, where
- 🔗 &&, ||
- 🔢 case (Rust doesn't use case, uses match arms)
- ❓ ? (Result unwrapping operator in Rust)

**Notes:** 
- `?` is for error propagation in Rust, not ternary
- ⚠️ `case` shouldn't be here, Rust doesn't have case keyword

---

### Scala (scala.py)
```python
_conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                   'case', 'do'])
```
- 🔀 if, for, while, catch, do
- 🔗 &&, ||
- 🔢 case (used in pattern matching)
- ❓ ?

---

### Solidity (solidity.py)
```python
_conditions = set(['if', 'for', 'while', '&&', '||', '?'])
```
- 🔀 if, for, while
- 🔗 &&, ||
- ❓ ?

---

### ST/Structured Text (st.py)
```python
_conditions = set([
    'if', 'elsif', 'case', 'for', 'while', 'repeat',
    'IF', 'ELSIF', 'CASE', 'FOR', 'WHILE', 'REPEAT'
])
```
- 🔀 IF/if, ELSIF/elsif, FOR/for, WHILE/while, REPEAT/repeat
- 🔢 CASE/case

**Notes:** 
- Case-insensitive language
- No logical operators included (uses AND/OR but not in _conditions)

---

### Swift (swift.py)
```python
_conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                  'case', 'guard'])
```
- 🔀 if, for, while, catch, guard
- 🔗 &&, ||
- 🔢 case
- ❓ ?

**Notes:** `guard` is Swift-specific early exit

---

### TNSDL (tnsdl.py)
```python
_conditions = set(['WHILE', 'AND', 'OR', '#if'])
```
- 🔀 WHILE, #if
- 🔗 AND, OR

**Notes:** Very limited set, uppercase only

---

### TSX (tsx.py)
```python
self._conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                       'catch', 'case'])
```
- 🔀 if, elseif, for, while, catch
- 🔗 &&, ||
- 🔢 case
- ❓ ?

---

### TTCN (ttcn.py)
```python
_conditions = set(['if', 'else', 'for', 'while',
                   'altstep', 'case', 'goto', 'alt',
                   'interleave', 'and', 'or', 'xor'])
```
- 🔀 if, else, for, while, altstep, alt, interleave, goto
- 🔗 and, or, xor
- 🔢 case

**Notes:** 
- `xor` is unusual
- `else` is unusual to include
- TTCN-specific: altstep, alt, interleave

---

### TypeScript (typescript.py)
```python
_conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                   'catch', 'case'])
```
- 🔀 if, elseif, for, while, catch
- 🔗 &&, ||
- 🔢 case
- ❓ ?

---

### Zig (zig.py)
```python
_conditions = {"if", "for", "while", "and", "or", "orelse", "try", "catch", "=>"}
```
- 🔀 if, for, while, try, catch
- 🔗 and, or, orelse
- ⚠️ => (Zig syntax for error union and switch cases)

**Notes:** `orelse` is Zig-specific error handling

---

## Summary Statistics

### Languages using symbol logical operators (&&, ||)
- C# ✓
- GDScript ✓
- Kotlin ✓
- Perl ✓ (also has word versions)
- PHP ✓
- Ruby/Lua ✓ (also has word versions)
- Rust ✓
- Scala ✓
- Solidity ✓
- Swift ✓
- TSX ✓
- TypeScript ✓

### Languages using word logical operators (and, or)
- Erlang ✓
- Fortran ✓ (.and., .or.)
- Perl ✓ (also has symbol versions)
- PL/SQL ✓
- Python ✓
- Ruby/Lua ✓ (also has symbol versions)
- TNSDL ✓
- TTCN ✓
- Zig ✓

### Languages with ternary operator (?)
- Base ✓
- C# ✓
- Erlang (different meaning)
- GDScript ✓
- Kotlin (?:, Elvis)
- Perl ✓
- PHP ✓
- Ruby/Lua ✓
- Rust (different meaning - unwrap)
- Scala ✓
- Solidity ✓
- Swift ✓
- TSX ✓
- TypeScript ✓

### Languages with case/switch
- Base (case) ✓
- C# (case) ✓
- Erlang (case, when) ✓
- Fortran (case) ✓
- GDScript (case) ✓
- PHP (case) ✓
- Rust (case - incorrect, match - correct) ⚠️
- Scala (case) ✓
- ST (case) ✓
- Swift (case) ✓
- TSX (case) ✓
- TTCN (case) ✓
- TypeScript (case) ✓

### Languages not overriding _conditions
These inherit the base class default:
- C++ (clike.py)
- Go (go.py)
- Java (java.py)
- JavaScript (javascript.py)
- Objective-C (objc.py)
- Vue (vue.py)

## Identified Issues

### High Priority
1. **R Language** - Mixing element-wise (`&`, `|`) with short-circuit (`&&`, `||`) operators
2. **Rust** - Includes `case` but Rust doesn't have case keyword
3. **Erlang** - `?` has different meaning (macro, not ternary)

### Medium Priority
4. **Perl** - Two different `_conditions` definitions in same file
5. **ST** - Missing logical operators AND/OR from _conditions
6. **TTCN** - Includes `else` which is unusual
7. **GDScript** - Includes `else` which is unusual

### Low Priority
8. **Go, Java, JavaScript, Vue** - Not overriding _conditions, inheriting base class
9. **Zig** - `=>` is language-specific, unclear categorization
10. **Ruby/Lua** - `when` might need special handling

## Recommendations

1. Create separate categories as proposed in main refactoring doc
2. Fix high-priority issues first (R, Rust, Erlang)
3. Audit languages not overriding _conditions
4. Add tests for edge cases in each language
5. Document rationale for ambiguous cases

