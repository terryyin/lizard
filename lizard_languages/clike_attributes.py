'''
Language attributes for C, C++ -like languages.
'''

# fmt off

""" Default C operators copied from https://www.w3schools.com/c/c_operators.php """
operators = {
    # Arithmetic operators
    "+_",   # Addition	        Adds together two values	x + y
    "-_",   # Subtraction	    Subtracts one value from another	x - y
    "*_",   # Multiplication	Multiplies two values	x * y
    "/_",   # Division	        Divides one value by another	x / y
    "%_",   # Modulus	        Returns the division remainder	x % y
    "++_",  # Increment	        Increases the value of a variable by 1	++x
    "--_",  # Decrement	        Decreases the value of a variable by 1	--x

    # Assignment operators
    "=",   # x = 5	x = 5
    "+=",  # x += 3	x = x + 3
    "-=",  # x -= 3	x = x - 3
    "*=",  # x *= 3	x = x * 3
    "/=",  # x /= 3	x = x / 3
    "%=",  # x %= 3	x = x % 3
    "&=",  # x &= 3	x = x & 3
    "|=",  # x |= 3	x = x | 3
    "^=",  # x ^= 3	x = x ^ 3
    ">>=",  # x >>= 3	x = x >> 3
    "<<=",  # x <<= 3	x = x << 3

    # Comparison operators
    "==", # Equal to	                Returns 1 if the values are equal
    "!=", # Not equal	                Returns 1 if the values are not equal
    ">",  # Greater than	            Returns 1 if the first value is greater than the second value
    "<",  # Less than	                Returns 1 if the first value is less than the second value
    ">=", # Greater than or equal to	Returns 1 if the first value is greater than, or equal to, the second value
    "<=", # Less than or equal to	    Returns 1 if the first value is less than, or equal to, the second value
    "&&", # AND	                        Returns 1 if both statements are true
    "||", # OR	                        Returns 1 if one of the statements is true
    "!",  # NOT	                        Reverse the result, returns 0 if the result is 1
    "&",  # AND	    Sets each bit to 1 if both bits are 1	x & y
    "|",  # OR	    Sets each bit to 1 if one of two bits is 1	x | y
    "^",  # XOR	    Sets each bit to 1 if only one of two bits is 1	x ^ y
    "~",  # NOT	    Inverts all the bits
    "<<", # Zero fill left shift	Shift left by pushing zeros in from the right and let the leftmost bits fall off	x << 2
    ">>", # Signed right shift	    Shift right by pushing copies of the leftmost bit in from the left, and let the rightmost bits fall off	x >> 2

}

""" Default C keywords copied from https://www.w3schools.com/c/c_ref_keywords.php """
keywords = {
    "alignas",  # (C23)
    "alignof",  # (C23)
    "auto",  #
    "bool",  # (C23)
    "break",  #
    "case",  #
    "char",  #
    "const",  #
    "constexpr",  # (C23)
    "continue",  #
    "default",  #
    "do",  #
    "double",  #
    "else",  #
    "enum",  #

    "extern",  #
    "false",  # (C23)
    "float",  #
    "for",  #
    "goto",  #
    "if",  #
    "inline",  # (C99)
    "int",  #
    "long",  #
    "nullptr",  # (C23)
    "register",  #
    "restrict",  # (C99)
    "return",  #
    "short",  #
    "signed",  #

    "sizeof",  #
    "static",  #
    "static_assert",  # (C23)
    "struct",  #
    "switch",  #
    "thread_local",  # (C23)
    "true",  # (C23)
    "typedef",  #
    "typeof",  # (C23)
    "typeof_unqual",  # (C23)
    "union",  #
    "unsigned",  #
    "void",  #
    "volatile",  #
    "while",  #

    "_Alignas",  # (C11)
    "_Alignof",  # (C11)
    "_Atomic",  # (C11)
    "_BitInt",  # (C23)
    "_Bool",  # (C99)
    "_Complex",  # (C99)
    "_Decimal128",  # (C23)
    "_Decimal32",  # (C23)
    "_Decimal64",  # (C23)
    "_Generic",  # (C11)
    "_Imaginary",  # (C99)
    "_Noreturn",  # (C11)
    "_Static_assert",  # (C11)
    "_Thread_local",  # (C11)
}

"""Expression grouping operators"""
expression_grouping = {
    # Brackets
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",

    # Comma
    ",",
}

# fmt on
