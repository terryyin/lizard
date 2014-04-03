#!/usr/bin/env python
# encoding: utf-8
'''
Setup script.
To install lizard:
[sudo] python setup.py install
'''
import lizard
from distutils.core import setup
def install(appname='lizard'):
    try:
        with open("README.rst") as f:
            long_description = f.read()
    except:
        long_description = lizard.__doc__  # @UndefinedVariable
    setup(
          name = appname,
          version = lizard.VERSION,
          description = ''' 
A simple code complexity analyzer without caring about the C/C++ header files or Java imports.
It can deal with Java/C/C++/JavaScript/Objective C code. It counts the cyclomatic complexity number etc.''',
          long_description =  long_description,
          url = 'https://github.com/terryyin/lizard',
          classifiers = ['Development Status :: 5 - Production/Stable',
                     'Intended Audience :: Developers',
                     'Intended Audience :: End Users/Desktop',
                     'License :: Freeware',
                     'Operating System :: POSIX',
                     'Operating System :: Microsoft :: Windows',
                     'Operating System :: MacOS :: MacOS X',
                     'Topic :: Software Development :: Quality Assurance',
                     'Programming Language :: C',
                     'Programming Language :: C++',
                     'Programming Language :: Java',
                     'Programming Language :: JavaScript',
                     'Programming Language :: Objective C',
                     'Programming Language :: Python',
                     'Programming Language :: Python :: 2.7',
                     'Programming Language :: Python :: 3.2',
                     'Programming Language :: Python :: 3.3'],
          packages = ['lizard_ext'],
          py_modules = ['lizard'],
          author = 'Terry Yin',
          author_email = 'terry@odd-e.com',
          scripts = ['lizard.bat', 'lizard', 'hfcca.bat', 'hfcca']
          )

if __name__ == "__main__":
    install()
