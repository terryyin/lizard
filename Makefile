.PHONY: all tests pep8 pylint deps test-deps publish

all: extensive pylint
extensive: tests pep8

tests:
	nosetests test

pep8:
	pep8 --exclude=mock.py lizard.py lizard_ext # test

pylint:
	pylint --rcfile pylintrc lizard.py lizard_ext

deps:
	pip install -r dev_requirements.txt

publish:
	python setup.py sdist upload

