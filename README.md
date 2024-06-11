# Report for Assignment 1


## Project chosen

Name: Lizard

URL: https://github.com/terryyin/lizard 


Number of lines of code and the tool used to count it: 57603 counted with lizard.py (same Github link)


Programming language: Python


## Coverage measurement


### Existing tool

We used coverage.py and entered the following command in the main directory of lizard: coverage run -m pytest test
this gave the following output:

![output pytest](/Screenshots/Output_pytest.png)

next we constructed the report by doing 'coverage report' and we downloaded this with 'coverage html'. the test also included some python packages that we ignored for this assigment and exluded them in the screenshots. the following two images are the result of 'coverage report' inside the terminal:

![output coverage report 0](/Screenshots/Coverage_report0.png)
![output coverage report 1](/Screenshots/Coverage_report1.png)
![output coverage report 2](/Screenshots/Coverage_report2.png)


### Your own coverage tool

## Daniel Buis

the first function I chose was the DefaultOrderedDict from default_ordered_dict.py in the lizard_ext directory based on its current coverage and complexity. (coverage of 65%)

this is a link to a commit made in our forked repository that shows the instrumented code to gather coverage measurements: https://github.com/terryyin/lizard/commit/4b4018e3c41bae26b1ed7a8cd375dccb6d0e1cde 

<Provide a screenshot of the coverage results output by the instrumentation>

<Function 2 name>

<Provide the same kind of information provided for Function 1>

## Kristóf Földházi

<Function 1 name>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the instrumented code to gather coverage measurements>

<Provide a screenshot of the coverage results output by the instrumentation>

<Function 2 name>

<Provide the same kind of information provided for Function 1>

## Darian de Graaf

<Function 1 name>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the instrumented code to gather coverage measurements>

<Provide a screenshot of the coverage results output by the instrumentation>

<Function 2 name>

<Provide the same kind of information provided for Function 1>

## Barnabas Turán

<Function 1 name>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the instrumented code to gather coverage measurements>

<Provide a screenshot of the coverage results output by the instrumentation>

<Function 2 name>

<Provide the same kind of information provided for Function 1>

## Coverage improvement

### Individual tests

## Daniel Buis

<Test 1>

this is a link to a commit made in our forked repository that shows the new/enhanced test:
https://github.com/terryyin/lizard/commit/6023cb2e157647e107c068485dccd0d48dd3f8b9 

The old coverage results:
![Old coverage 1](Screenshots/Old_coverage_Daniel1.png)

The new coverage results:
![improved coverage 1](/Screenshots/Improved_Coverage_Daniel1.png)

<State the coverage improvement with a number and elaborate on why the coverage is improved>

<Test 2>

<Provide the same kind of information provided for Test 1>

## Kristóf Földházi

<Test 1>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the new/enhanced test>

<Provide a screenshot of the old coverage results (the same as you already showed above)>

<Provide a screenshot of the new coverage results>

<State the coverage improvement with a number and elaborate on why the coverage is improved>

<Test 2>

<Provide the same kind of information provided for Test 1>

## Darian de Graaf

<Test 1>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the new/enhanced test>

<Provide a screenshot of the old coverage results (the same as you already showed above)>

<Provide a screenshot of the new coverage results>

<State the coverage improvement with a number and elaborate on why the coverage is improved>

<Test 2>

<Provide the same kind of information provided for Test 1>

## Barnabas Turán

<Test 1>

<Show a patch (diff) or a link to a commit made in your forked repository that shows the new/enhanced test>

<Provide a screenshot of the old coverage results (the same as you already showed above)>

<Provide a screenshot of the new coverage results>

<State the coverage improvement with a number and elaborate on why the coverage is improved>

<Test 2>

<Provide the same kind of information provided for Test 1>

### Overall


<Provide a screenshot of the old coverage results by running an existing tool (the same as you already showed above)>


<Provide a screenshot of the new coverage results by running the existing tool using all test modifications made by the group>


## Statement of individual contributions


<Write what each group member did>