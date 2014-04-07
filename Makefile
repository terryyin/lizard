all: tests

tests:
	python -munittest test

pep8:
	pep8 lizard.py | head
