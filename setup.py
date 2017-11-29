#!/usr/bin/env python
'''
Setup script.
To install lizard:
sudo setup.py build install
'''

import codecs
import os
import re
from setuptools import setup, Command

here = os.path.dirname(os.path.abspath(__file__))
version = '0.0.0'
changes = os.path.join(here, "CHANGELOG.md")
pattern = r'^\#*\s*(?P<version>[0-9]+.[0-9]+(.[0-9]+)?)'
with codecs.open(changes, encoding='utf-8') as changes:
    for line in changes:
        match = re.match(pattern, line)
        if match:
            version = match.group("version")
            break


class VersionCommand(Command):
    description = 'Show library version'
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        print(version)

# Save last Version
def save_version():
    version_path = os.path.join(here, "lizard_ext/version.py")

    with open(version_path) as version_file_read:
        content_file = version_file_read.read()

    VSRE = r"^version = ['\"]([^'\"]*)['\"]"
    mo = re.search(VSRE, content_file, re.M)
    current_version = mo.group(1)

    content_file = content_file.replace(current_version, "{}".format(version))

    with open(version_path, 'w') as version_file_write:
        version_file_write.write(content_file)


save_version()



setup(
    name='lizard',
    version=version,
    description='''A code analyzer without caring the C/C++ header files. ''' +
        '''It works with Java, C/C++, JavaScript, Python, Ruby, Swift, Objective C. Metrics includes cyclomatic complexity number etc.''',
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
    cmdclass={'version': VersionCommand},
    packages=['lizard_ext', 'lizard_languages'],
    py_modules=['lizard'],
    entry_points={'console_scripts': ['lizard = lizard:main']},
    author='Terry Yin',
    author_email='terry@odd-e.com',
)
