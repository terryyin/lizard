all: tests pep8
extensive: all pylint

tests:
	python -munittest test

pep8:
	pep8 lizard.py lizard_ext

pylint:
	pylint --rcfile pylintrc lizard.py lizard_ext
