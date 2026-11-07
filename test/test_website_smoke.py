"""Observe startup waiting over real HTTP, with the real Flask homepage."""

import importlib.util
import io
import json
import shutil
import threading
from contextlib import contextmanager
from pathlib import Path
from urllib.error import HTTPError
from wsgiref.simple_server import make_server

import pytest

import index

SPEC = importlib.util.spec_from_file_location(
    "website_smoke", Path(__file__).resolve().parents[1] / "scripts" / "website_smoke.py")
smoke = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(smoke)


@pytest.fixture
def built(tmp_path, monkeypatch):
    static = tmp_path / "static"
    shutil.copytree(index.app.static_folder, static, ignore=shutil.ignore_patterns("dist"))
    dist = static / "dist"
    (dist / "assets").mkdir(parents=True)
    (dist / "manifest.json").write_text(json.dumps({"src/main.jsx": {
        "file": "assets/main.js", "css": ["assets/main.css"], "isEntry": True}}))
    (dist / "assets/main.js").write_text("export {};")
    (dist / "assets/main.css").write_text("body {}")
    monkeypatch.setattr(index, "BUILD_DIR", dist)
    monkeypatch.setattr(index.app, "static_folder", str(static))
    return static


@contextmanager
def homepage_server(failures, status="500 Internal Server Error"):
    requests = []

    def application(environ, start_response):
        requests.append(environ["PATH_INFO"])
        if failures is None or len(requests) <= failures:
            start_response(status, [("Content-Type", "text/plain")])
            return [b"App Engine is not ready to dispatch"]
        return index.app(environ, start_response)

    with http_server(application) as url:
        yield url, requests


@contextmanager
def http_server(application):
    server = make_server("127.0.0.1", 0, application)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield "http://127.0.0.1:%s" % server.server_port
    finally:
        server.shutdown()
        server.server_close()
        thread.join(timeout=5)


def test_transient_5xx_waits_then_observes_real_healthy_homepage(built):
    with homepage_server(failures=1) as (url, requests):
        page = smoke.wait_until_ready(url, timeout=2, interval=0.01)
    assert b"Lizard" in page and b'id="lizard-config"' in page
    assert requests == ["/", "/"]


def test_persistent_5xx_stops_at_readiness_deadline():
    with homepage_server(failures=None) as (url, _):
        with pytest.raises(TimeoutError, match="readiness deadline exceeded.*HTTP Error 500"):
            smoke.wait_until_ready(url, timeout=0.08, interval=0.02)


def test_non_5xx_error_is_not_hidden_by_startup_wait():
    with homepage_server(failures=None, status="404 Not Found") as (url, requests):
        with pytest.raises(HTTPError) as error:
            smoke.wait_until_ready(url, timeout=2, interval=0.01)
    assert error.value.code == 404
    assert requests == ["/"]


def test_deployment_verification_still_requires_normal_https():
    with homepage_server(failures=None) as (url, requests):
        with pytest.raises(ValueError, match="normal HTTPS"):
            smoke.verify(url)
    assert requests == []


def test_application_check_fetches_built_assets_original_logos_and_real_analysis(built):
    with homepage_server(failures=0) as (url, requests):
        result = smoke.check_site(url, smoke.wait_until_ready(url))
    function = result["function_list"][0]
    assert (function["cyclomatic_complexity"], function["nloc"], function["parameter_count"]) == (3, 6, 1)
    assert {"/website/static/dist/assets/main.js", "/website/static/dist/assets/main.css",
            "/website/static/img/logo.png", "/website/static/img/logo-small.png",
            "/website/static/favicon.ico", "/analyze"}.issubset(requests)


def test_missing_generated_asset_rejects_an_otherwise_healthy_application(built):
    (built / "dist/assets/main.css").unlink()
    with homepage_server(failures=0) as (url, requests):
        with pytest.raises(HTTPError) as error:
            smoke.check_site(url, smoke.wait_until_ready(url))
    assert error.value.code == 404
    assert "/analyze" not in requests


def test_unexpected_real_analyzer_metrics_reject_the_application(built):
    def application(environ, start_response):
        if environ["PATH_INFO"] == "/analyze":
            # The transport fixture submits different real source; analyzer is unchanged.
            body = json.dumps({"lang": ".py", "code": "def answer():\n    return 42\n"}).encode()
            environ["wsgi.input"] = io.BytesIO(body)
            environ["CONTENT_LENGTH"] = str(len(body))
        return index.app(environ, start_response)

    with http_server(application) as url:
        with pytest.raises(ValueError, match="unexpected sample metrics"):
            smoke.check_site(url, smoke.wait_until_ready(url))


def test_http_redirect_preserves_https_host_path_and_query():
    def application(environ, start_response):
        location = "https://" + environ["HTTP_HOST"] + environ["PATH_INFO"] + "?" + environ["QUERY_STRING"]
        start_response("301 Moved Permanently", [("Location", location)])
        return []

    with http_server(application) as url:
        smoke.verify_redirect(url.replace("http:", "https:", 1))


@pytest.mark.parametrize("location", ["http://example.test/wrong", "https://example.test/wrong"])
def test_http_redirect_to_another_path_or_scheme_is_rejected(location):
    def application(environ, start_response):
        start_response("301 Moved Permanently", [("Location", location)])
        return []

    with http_server(application) as url:
        with pytest.raises(ValueError, match="expected HTTPS path and query"):
            smoke.check_redirect(url + "/expected?keep=1", "https://example.test/expected?keep=1")


def test_http_200_is_not_accepted_as_a_https_redirect():
    def application(environ, start_response):
        start_response("200 OK", [])
        return [b"healthy but insecure"]

    with http_server(application) as url:
        with pytest.raises(ValueError, match="did not redirect"):
            smoke.check_redirect(url, "https://example.test/")
