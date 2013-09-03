#!/usr/bin/env python
# encoding: utf-8
'''
Setup script.
To install hfcca:
[sudo] python setup.py install
'''
import os
from os.path import join
import hfcca
from distutils.core import setup
def install():
    try:
        long_description = open("README.md").read()
    except:
        long_description = hfcca.__doc__
    setup(
          name = 'hfcca',
          version = hfcca.VERSION,
          description = ''' 
source_analyzer is a simple code complexity source_file_counter without caring about the C/C++ header files.
It can deal with C/C++/Objective C & TNSDL code. It count the NLOC (lines of code without comments), CCN 
(cyclomatic complexity number) and token count of _functions.''',
          long_description =  long_description,
          url = 'https://github.com/terryyin/hfcca',
          classifiers = ['Development Status :: 5 - Production/Stable',
                     'Intended Audience :: Developers',
                     'Intended Audience :: End Users/Desktop',
                     'License :: Freeware',
                     'Operating System :: POSIX',
                     'Operating System :: Microsoft :: Windows',
                     'Operating System :: MacOS :: MacOS X',
                     'Topic :: Software Development :: Quality Assurance',
                     'Programming Language :: Python',
                     'Programming Language :: Python :: 2.6',
                     'Programming Language :: Python :: 2.7',
                     'Programming Language :: Python :: 3.2',
                     'Programming Language :: Python :: 3.3'],
          py_modules = ['hfcca'],
          author = 'Terry Yin',
          author_email = 'terry.yinze@gmail.com',
          #scripts = ['hfcca.bat' if os.sep == '\\' else 'hfcca']
          scripts = ['hfcca.bat', 'hfcca']
          )

if __name__ == "__main__":
    install()
