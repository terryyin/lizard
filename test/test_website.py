"""Observe the website through HTTP requests using the real analyzer."""

import pytest

from index import app
from website import application


@pytest.fixture
def client():
    return app.test_client()


def test_homepage_and_existing_wsgi_import(client):
    response = client.get("/")
    assert response.status_code == 200
    assert application is app
    page = response.get_data(as_text=True)
    for content in ("Understand", "pip install lizard", "lizard src/",
                    "https://pypi.org/project/lizard/",
                    "https://github.com/terryyin/lizard/blob/master/docs/cli.rst",
                    'id="analysis-form"', 'aria-live="polite"',
                    'name="viewport"', 'data-max-source-bytes="65536"',
                    'data-source-limit-message="The online demo accepts up to 64 KiB',
                    "is not saved"):
        assert content in page
    assert '<option value=".py" selected>Python</option>' in page
    assert "bower" not in page
    assert "script-src 'self'" in response.headers["Content-Security-Policy"]


@pytest.mark.parametrize("path,mimetype", [
    ("/website/static/img/logo.png", "image/png"),
    ("/website/static/img/logo-small.png", "image/png"),
    ("/website/static/favicon.ico", "image/"),
    ("/favicon.ico", "image/"),
    ("/website/static/site.css", "text/css"),
    ("/website/static/demo.js", "javascript"),
])
def test_local_assets_and_published_logo_urls(client, path, mimetype):
    response = client.get(path)
    assert response.status_code == 200
    assert mimetype in response.mimetype
    assert response.data


@pytest.mark.parametrize("language,code,name,parameters,nloc", [
    (".py", "def classify(value):\n    if value > 0:\n        return 1\n    return 0\n",
     "classify", 1, 4),
    ("example.cpp", "int classify(int value) { if (value > 0) return 1; return 0; }",
     "classify", 1, 1),
    (".js", "function classify(value) { if (value > 0) return 1; return 0; }",
     "classify", 1, 1),
])
def test_analysis_returns_actual_function_metrics(client, language, code, name,
                                                 parameters, nloc):
    response = client.post("/analyze", json={"lang": language, "code": code})
    assert response.status_code == 200
    result = response.get_json()
    assert result["filename"] == language
    assert result["nloc"] == nloc
    assert result["token_count"] > 0
    assert len(result["function_list"]) == 1
    function = result["function_list"][0]
    assert function["name"] == name
    assert function["cyclomatic_complexity"] == 2
    assert function["parameter_count"] == parameters
    assert function["nloc"] == nloc
    assert function["token_count"] > 0
    assert function["start_line"] == 1
    assert function["end_line"] >= function["start_line"]
    assert response.headers["Cache-Control"] == "no-store"


def test_filename_input_and_non_executable_source(client, capsys):
    source = 'def example():\n    raise RuntimeError("PRIVATE_SOURCE")\n'
    response = client.post("/analyze", json={"filename": "example.py", "code": source})
    assert response.status_code == 200
    assert response.get_json()["function_list"][0]["name"] == "example"
    assert "PRIVATE_SOURCE" not in response.get_data(as_text=True)
    captured = capsys.readouterr()
    assert "PRIVATE_SOURCE" not in captured.out + captured.err


def test_legacy_default_language_and_form_alias_use_the_same_adapter(client):
    code = "int answer() { return 42; }"
    legacy = client.post("/analyze", json={"code": code})
    form = client.post("/analyse", data={"content": code})
    assert legacy.status_code == form.status_code == 200
    assert legacy.get_json()["function_list"] == form.get_json()["function_list"]
    assert legacy.get_json()["filename"] == ".c"
    assert form.get_json()["filename"] == ".cpp"


@pytest.mark.parametrize("payload", [
    [], None, {"code": ""}, {"code": "   "}, {"code": 123},
    {"code": "PRIVATE_SOURCE", "lang": "unknown.extension"},
    {"code": "PRIVATE_SOURCE", "lang": []},
    {"code": "PRIVATE_SOURCE", "filename": "a" * 129 + ".py"},
    {"code": "\ud800", "lang": ".py"},
])
def test_invalid_input_is_a_safe_client_error(client, payload):
    response = client.post("/analyze", data=app.json.dumps(payload),
                           content_type="application/json")
    assert response.status_code == 400
    assert response.get_json()["error"]
    assert "PRIVATE_SOURCE" not in response.get_data(as_text=True)
    assert "Traceback" not in response.get_data(as_text=True)


def test_malformed_json_and_wrong_content_type(client):
    malformed = client.post("/analyze", data='{"code": PRIVATE_SOURCE',
                            content_type="application/json")
    assert malformed.status_code == 400
    assert malformed.get_json() == {"error": "The request contains invalid JSON."}
    plain = client.post("/analyze", data="PRIVATE_SOURCE", content_type="text/plain")
    assert plain.status_code == 415
    assert "PRIVATE_SOURCE" not in plain.get_data(as_text=True)


@pytest.mark.parametrize("character,count", [("x", 64 * 1024 + 1), ("é", 32 * 1024 + 1)])
def test_source_limit_counts_utf8_bytes_and_allows_retry(client, character, count):
    code = character * count
    response = client.post("/analyze", json={"lang": ".py", "code": code})
    assert response.status_code == 413
    assert "64 KiB" in response.get_json()["error"]
    retry = client.post("/analyze", json={"lang": ".py", "code": "def ok():\n    return 1\n"})
    assert retry.status_code == 200
    assert retry.get_json()["function_list"][0]["name"] == "ok"


def test_request_body_limit_is_json_and_source_is_not_reflected(client):
    response = client.post("/analyze", data="PRIVATE_SOURCE" * (512 * 1024 // 14 + 1),
                           content_type="application/json")
    assert response.status_code == 413
    assert "64 KiB" in response.get_json()["error"]
    assert "PRIVATE_SOURCE" not in response.get_data(as_text=True)


def test_source_at_the_documented_limit_is_accepted(client):
    code = "#" + "x" * (64 * 1024 - 1)
    response = client.post("/analyze", json={"lang": ".py", "code": code})
    assert response.status_code == 200
    assert response.get_json()["function_list"] == []


def test_source_without_functions_is_a_valid_empty_report(client):
    response = client.post("/analyze", json={"lang": ".py", "code": "answer = 42"})
    assert response.status_code == 200
    assert response.get_json()["function_list"] == []
