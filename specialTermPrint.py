"""
Special terminal printing options (bold, red, green...)
"""


color = {
    "PURPLE": '\033[95m',
    "CYAN": '\033[96m',
    "DARKCYAN": '\033[36m',
    "BLUE": '\033[94m',
    "GREEN": '\033[92m',
    "YELLOW": '\033[93m',
    "RED": '\033[91m',
    "BOLD": '\033[1m',
    "UNDERLINE": '\033[4m',
    "END": '\033[0m',
}


def bold_str(string: str) -> str:
    return color["BOLD"] + string + color["END"]


def red_str(string: str) -> str:
    return color["RED"] + string + color["END"]


def green_str(string: str) -> str:
    return color["GREEN"] + string + color["END"]


def red_bold_str(string: str) -> str:
    return bold_str(red_str(string))


def green_bold_str(string: str) -> str:
    return bold_str(green_str(string))
