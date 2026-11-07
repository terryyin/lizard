"""Observe the website through HTTP requests using the real analyzer."""

import json
import re

import pytest

import index
from index import app
from website import application


@pytest.fixture
def client():
    return app.test_client()


@pytest.fixture
def built(tmp_path, monkeypatch):
    """A filesystem stand-in for the Vite build output that Flask reads."""
    (tmp_path / "manifest.json").write_text(json.dumps({
        "src/main.jsx": {"file": "assets/main-abc.js", "isEntry": True,
                         "css": ["assets/main-def.css"]},
        "src/other.js": {"file": "assets/other-ghi.js"},
    }))
    monkeypatch.setattr(index, "BUILD_DIR", tmp_path)
    return tmp_path


def bootstrap_data(page):
    match = re.search(r'<script type="application/json" id="lizard-config">(.*?)</script>',
                      page, re.DOTALL)
    return json.loads(match.group(1))


def test_homepage_is_a_shell_for_the_manifest_selected_bundle(client, built):
    response = client.get("/")
    assert response.status_code == 200
    page = response.get_data(as_text=True)
    assert '<script type="module" src="/website/static/dist/assets/main-abc.js"></script>' in page
    assert '<link rel="stylesheet" href="/website/static/dist/assets/main-def.css">' in page
    assert "other-ghi.js" not in page
    assert '<div id="root"></div>' in page
    assert '<link rel="icon" href="/website/static/favicon.ico">' in page
    assert 'name="viewport"' in page and 'name="description"' in page
    assert "<noscript>" in page and "docs/cli.rst" in page
    assert "script-src 'self'" in response.headers["Content-Security-Policy"]
    assert application is app


def test_homepage_bootstraps_server_owned_languages_and_limits(client, built):
    config = bootstrap_data(client.get("/").get_data(as_text=True))
    assert ["Python", ".py"] in config["languages"]
    assert ["C / C++", ".c"] in config["languages"]
    assert config["languages"] == sorted(config["languages"])
    assert config["maxSourceBytes"] == 65536
    assert config["sourceLimitMessage"].startswith("The online demo accepts up to 64 KiB")


def test_bootstrap_data_cannot_break_out_of_its_script_element(client, built, monkeypatch):
    hostile = "</script><script>alert(1)</script><!--"
    monkeypatch.setattr(index, "SOURCE_LIMIT_MESSAGE", hostile)
    page = client.get("/").get_data(as_text=True)
    assert "<script>alert(1)" not in page
    assert bootstrap_data(page)["sourceLimitMessage"] == hostile


@pytest.mark.parametrize("manifest", [None, "{not json", '{"src/main.jsx": {"file": "a.js"}}'])
def test_unbuilt_homepage_explains_the_build_while_the_api_works(
        client, tmp_path, monkeypatch, manifest):
    if manifest is not None:
        (tmp_path / "manifest.json").write_text(manifest)
    monkeypatch.setattr(index, "BUILD_DIR", tmp_path)
    response = client.get("/")
    assert response.status_code == 503
    assert response.mimetype == "text/plain"
    assert "npm --prefix website/frontend run build" in response.get_data(as_text=True)
    analysis = client.post("/analyze", json={"lang": ".py", "code": "def ok():\n    return 1\n"})
    assert analysis.status_code == 200
    assert analysis.get_json()["function_list"][0]["name"] == "ok"


@pytest.mark.parametrize("path,mimetype", [
    ("/website/static/img/logo.png", "image/png"),
    ("/website/static/img/logo-small.png", "image/png"),
    ("/website/static/favicon.ico", "image/"),
    ("/favicon.ico", "image/"),
])
def test_published_logo_and_favicon_urls(client, path, mimetype):
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
