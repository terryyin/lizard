#!/bin/bash

echo Show the environment
pip freeze

echo Install setup.py
python setup.py build install

make
