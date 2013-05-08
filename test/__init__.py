import sys
if sys.version_info.major > 2:
    from .testHfcca import *
else:
    #for early version of python (2.5)
    from testHfcca import *
