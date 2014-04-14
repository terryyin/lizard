all: extensive pylint
extensive: tests pep8

tests:
	python -munittest test

pep8:
	pep8 --exclude=mock.py lizard.py lizard_ext # test

pylint:
	pylint --rcfile pylintrc lizard.py lizard_ext
