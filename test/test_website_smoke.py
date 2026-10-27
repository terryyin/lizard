"""Observe startup waiting over real HTTP, with the real Flask homepage."""

import importlib.util
import json
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
    (tmp_path / "manifest.json").write_text(json.dumps({
        "src/main.jsx": {"file": "assets/main.js", "isEntry": True}}))
    monkeypatch.setattr(index, "BUILD_DIR", tmp_path)


@contextmanager
def homepage_server(failures, status="500 Internal Server Error"):
    requests = []

    def application(environ, start_response):
        requests.append(environ["PATH_INFO"])
        if failures is None or len(requests) <= failures:
            start_response(status, [("Content-Type", "text/plain")])
            return [b"App Engine is not ready to dispatch"]
        return index.app(environ, start_response)

    server = make_server("127.0.0.1", 0, application)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield "http://127.0.0.1:%s" % server.server_port, requests
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
