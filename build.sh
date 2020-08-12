#!/bin/bash

echo Setup virtual environment
python -m venv venv

echo Show the environment
pip freeze

echo Install setup.py
python setup.py build install

make
