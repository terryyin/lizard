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

I chose to measure the coverage from 2 functions in fortran.py in the lizard languages directory.
The first function I chose to measure the coverage on is _state_global and the second function is _ignore_if_paren. Neither of these functions were fully covered by the original tests.

In this commit you can see the code i added to measure the coverage of these functions. https://github.com/terryyin/lizard/commit/1c4e0a83567b86584c5368737235e8bd6cf90a59

In this screenshot you can see the coverage results of these changes resulting in a coverage of 44% over these 2 functions and a total coverage of 87% on this file as shown in the screenshots in the existing tool section.
![output own coverage tool Darian](/Screenshots/Coverage_personal_tool_Darian.png)


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
The first test i made is called test_new_block. In the following commit you can see the test from line 62-82.
https://github.com/terryyin/lizard/commit/762b627af59d9cf2ae9b288f29c4d05f8bae8172

The old coverage results
![output own coverage tool Darian](/Screenshots/Coverage_fortran_before.png)

The new coverage results
![output own coverage tool Darian](/Screenshots/Coverage_fortran_after1.png)

Before the coverage was 87% on the fortran.py file and after the first new test the coverage is improved to 89%.
This happened because the 2 functions mentioned earlier in the personal tool measurement _state_global and _ignore_if_paren had a total of 3 extra branches covered.
One branch in _state_global for the BLOCK token and both branches in the _ignore_if_paren function are covered by this test.

<Test 2>
The second test i made is called test_program. In the following commit you can see the new test from line 69-86
https://github.com/terryyin/lizard/commit/18f006521925030094da6b0c6d30bfcf9976146d

The old coverage results
![output own coverage tool Darian](/Screenshots/Coverage_fortran_before.png)

The new coverage results
![output own coverage tool Darian](/Screenshots/Coverage_fortran_after2.png)

Before the second new test the coverage was 89% as stated above and after the second new test the coverage is improved to 91%.
The new test covers one branch in the _state_global function, namely the PROGRAM branch, and tests if this branch is properly executed.

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