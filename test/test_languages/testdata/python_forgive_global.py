# Global code with complexity
x = 1
if x > 0:
    print("Positive")
elif x < 0:
    print("Negative")
else:
    print("Zero")

# #lizard forgive global
# More global code with complexity
y = 2
if y > 0:
    print("Y is positive")
elif y < 0:
    print("Y is negative")

# This function should still be counted
def test_function(param):
    if param > 0:
        print("Param is positive")
    elif param < 0:
        print("Param is negative")
    else:
        print("Param is zero") 