from flask import Flask, request, url_for, render_template
from lizard import analyze_file
import os
app = Flask(__name__)
app.config['DEBUG'] = True


@app.route('/')
def hello():
    return render_template('index.html')


@app.route('/analyse', methods=['POST'])
def analyse():
    return render_template('index.html', info=analyze_file.analyze_source_code("a.cpp", request.form['content']))


@app.errorhandler(404)
def page_not_found(e):
    return 'Sorry, nothing at this URL.', 404

