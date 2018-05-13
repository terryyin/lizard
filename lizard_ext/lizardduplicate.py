'''
Get Duplicated parameter lists
'''
from collections import Counter
from .extension_base import ExtensionBase


class LizardExtension(ExtensionBase):

    def __init__(self, context=None):
        self.duplicates = []
        super(LizardExtension, self).__init__(context)
