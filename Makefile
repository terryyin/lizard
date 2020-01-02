.PHONY: all tests pep8 pylint deps test-deps publish

all: extensive pylint
extensive: tests pep8

tests:
	py.test test

tests3:
	python3 -m unittest test

pep8:
	pycodestyle lizard.py lizard_ext lizard_languages

pylint:
	pylint --exit-zero --rcfile pylintrc lizard.py lizard_ext lizard_languages

deps:
	pip3 install -r dev_requirements.txt

pip-upgrade:
	pip3 install --upgrade -r dev_requirements.txt

build: test
	python3 setup.py sdist
	python3 setup.py bdist_wheel

release: build
	git tag `python setup.py -q version`
	git push origin `python setup.py -q version`
	twine upload dist/*

clean: clean-eggs clean-build
	@find . -iname '*.pyc' -delete
	@find . -iname '*.pyo' -delete
	@find . -iname '*~' -delete
	@find . -iname '*.swp' -delete
	@find . -iname '__pycache__' -delete

clean-eggs:
	@find . -name '*.egg' -print0|xargs -0 rm -rf --
	@rm -rf .eggs/

clean-build:
	@rm -fr build/
	@rm -fr dist/
	@rm -fr *.egg-info
