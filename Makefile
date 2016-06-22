.PHONY: all tests pep8 pylint deps test-deps publish

all: extensive pylint
extensive: tests pep8

tests:
	nosetests test

tests3:
	python3 -m unittest test

pep8:
	pycodestyle lizard.py lizard_ext lizard_languages # test

pylint:
	pylint --rcfile pylintrc lizard.py lizard_ext lizard_languages

deps:
	pip install -r dev_requirements.txt

publish:
	python setup.py sdist bdist_wheel upload

