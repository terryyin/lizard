#!/usr/bin/env python
'''
Setup script.
To install lizard:
sudo setup.py build install
'''

from setuptools import setup

import lizard

setup(
    name='lizard',
    version=lizard.VERSION,
    description='''A code analyzer without caring the C/C++ header files.
It works with Java, C/C++, JavaScript, Python, Ruby, Swift, Objective C. Metrics includes cyclomatic complexity number etc.''',
    long_description=open('README.rst').read(),
    url='http://www.lizard.ws',
    download_url='https://pypi.python.org/lizard/',
    license='MIT',
    platforms='any',
    classifiers=['Development Status :: 5 - Production/Stable',
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
                    'Programming Language :: Python :: 3.3',
                    'Programming Language :: Python :: 3.4',
                    'Programming Language :: Python :: 3.5'],
    packages=['lizard_ext', 'lizard_languages'],
    py_modules=['lizard'],
    entry_points={'console_scripts': ['lizard = lizard:main']},
    author='Terry Yin',
    author_email='terry@odd-e.com',
)
