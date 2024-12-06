'''
Language attributes for Python.
'''

# fmt off

""" Keywords copied from https://www.w3schools.com/python/python_ref_keywords.asp """

keywords = {
    "and",      # A logical operator
    "as",       # To create an alias
    "assert",   # For debugging
    "break",    # To break out of a loop
    "class",    # To define a class
    "continue", # To continue to the next iteration of a loop
    "def",      # To define a function
    "del",      # To delete an object
    "elif",     # Used in conditional statements, same as else if
    "else",     # Used in conditional statements
    "except",   # Used with exceptions, what to do when an exception occurs
    "False",    # Boolean value, result of comparison operations
    "finally",  # Used with exceptions, a block of code that will be executed no matter if there is an exception or not
    "for",      # To create a for loop
    "from",     # To import specific parts of a module
    "global",   # To declare a global variable
    "if",       # To make a conditional statement
    "import",   # To import a module
    "in",       # To check if a value is present in a list, tuple, etc.
    "is",       # To test if two variables are equal
    "lambda",   # To create an anonymous function
    "None",     # Represents a null value
    "nonlocal", # To declare a non-local variable
    "not",      # A logical operator
    "or",       # A logical operator
    "pass",     # A null statement, a statement that will do nothing
    "raise",    # To raise an exception
    "return",   # To exit a function and return a value
    "True",     # Boolean value, result of comparison operations
    "try",      # To make a try...except statement
    "while",    # To create a while loop
    "with",     # Used to simplify exception handling
    "yield",    # To return a list of values from a generator
}

""" Operator copied from https://www.w3schools.com/python/python_operators.asp """
operators = {
    "+",  # Addition	    x + y
    "-",  # Subtraction	    x - y
    "*",  # Multiplication	x * y
    "/",  # Division	    x / y
    "%",  # Modulus	        x % y
    "**",  # Exponentiation	x ** y
    "//",  # Floor division	x // y
    "= ",  # Assignment      x = 5	x = 5
    "+= ",  # Assignment      x += 3	x = x + 3
    "-= ",  # Assignment      x -= 3	x = x - 3
    "*= ",  # Assignment      x *= 3	x = x * 3
    "/= ",  # Assignment      x /= 3	x = x / 3
    "%= ",  # Assignment      x %= 3	x = x % 3
    "//=",  # Assignment      x //= 3	x = x // 3
    "**=",  # Assignment      x **= 3	x = x ** 3
    "&= ",  # Assignment      x &= 3	x = x & 3
    "|= ",  # Assignment      x |= 3	x = x | 3
    "^= ",  # Assignment      x ^= 3	x = x ^ 3
    ">>=",  # Assignment      x >>= 3	x = x >> 3
    "<<=",  # Assignment      x <<= 3	x = x << 3
    ":= ",  # Assignment      print(x := 3)	x = 3
    "==",  # Equal	x == y
    "!=",  # Not equal	x != y
    ">",  # Greater than	x > y
    "<",  # Less than	x < y
    ">=",  # Greater than or equal to	x >= y
    "<=",  # Less than or equal to	x <= y
    "and",  # Returns True if both statements are true	x < 5 and  x < 10
    "or",  # Returns True if one of the statements is true	x < 5 or x < 4
    "not",  # Reverse the result, returns False if the result is true	not(x < 5 and x < 10)
    "is",  # Returns True if both variables are the same object	x is y
    "is not",  # Returns True if both variables are not the same object	x is not y
    "in",  # Returns True if a sequence with the specified value is present in the object	x in y
    "not in", # Returns True if a sequence with the specified value is not present in the object	x not in y
    "&",  # AND	Sets each bit to 1 if both bits are 1	x & y
    "|",  # OR	Sets each bit to 1 if one of two bits is 1	x | y
    "^",  # XOR	Sets each bit to 1 if only one of two bits is 1	x ^ y
    "~",  # NOT	Inverts all the bits	~x
    "<<", # Zero fill left shift	Shift left by pushing zeros in from the right and let the leftmost bits fall off	x << 2
    ">>", # Signed right shift	Shift right by pushing copies of the leftmost bit in from the left, and let the rightmost bits fall off	x >> 2
}

""" Expression grouping operators """
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
