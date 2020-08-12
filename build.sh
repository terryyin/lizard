#!/bin/bash

echo Create Virtual Environment
python3 -m venv venv

echo Activate Virtual Environment
source venv/bin/activate

# Why is development mode necessary ?
pip install -e .

echo Prepare requirements within the Virtual Environments
venv/bin/pip install -r dev_requirements.txt

echo Show the environment
pip freeze

echo Install setup.py
python setup.py build install

make

echo Deactivate Virtual Environment
deactivate

