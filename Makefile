all: tests pep8

tests:
	python -munittest test

pep8:
	pep8 lizard.py lizard_ext

pylint:
	pylint --rcfile pylintrc lizard.py
