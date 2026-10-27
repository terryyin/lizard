"""Temporary release observations against an explicitly supplied HTTPS URL."""

import argparse
import datetime
import json
import urllib.error
import urllib.request


SAMPLE = (
    "def shipping_cost(order):\n"
    "    if order.is_pickup:\n"
    "        return 0\n"
    "    if order.total >= 50:\n"
    "        return 0\n"
    "    return 5"
)


def request(base, path, data=None, content_type=None):
    headers = {"Cache-Control": "no-cache"}
    if content_type:
        headers["Content-Type"] = content_type
    req = urllib.request.Request(base.rstrip("/") + path, data=data, headers=headers)
    try:
        response = urllib.request.urlopen(req, timeout=30)
    except urllib.error.HTTPError as error:
        response = error
    with response:
        return response.status, response.headers, response.read()


def analyze(base):
    status, headers, body = request(
        base, "/analyze", json.dumps({"lang": ".py", "code": SAMPLE}).encode(),
        "application/json",
    )
    assert status == 200, ("analysis status", status)
    result = json.loads(body)
    assert result["filename"] == ".py"
    assert len(result["function_list"]) == 1
    function = result["function_list"][0]
    assert function["name"] == "shipping_cost"
    assert function["cyclomatic_complexity"] == 3
    assert function["nloc"] == 6
    assert function["parameter_count"] == 1
    assert function["token_count"] > 0
    assert headers["Cache-Control"] == "no-store"
    return {key: function[key] for key in (
        "name", "cyclomatic_complexity", "nloc", "parameter_count", "token_count"
    )}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("base")
    args = parser.parse_args()
    assert args.base.startswith("https://")
    status, headers, page = request(args.base, "/")
    assert status == 200, ("homepage status", status)
    assert b"Understand" in page and b"pip install lizard" in page
    assert b'id="analysis-form"' in page
    assert "script-src 'self'" in headers["Content-Security-Policy"]
    assets = {}
    for path, content_type in (
        ("/website/static/site.css", "text/css"),
        ("/website/static/demo.js", "javascript"),
        ("/website/static/img/logo.png", "image/png"),
        ("/website/static/img/logo-small.png", "image/png"),
        ("/website/static/favicon.ico", "image/"),
        ("/favicon.ico", "image/"),
    ):
        status, asset_headers, body = request(args.base, path)
        assert status == 200, (path, status)
        assert content_type in asset_headers["Content-Type"], (path, dict(asset_headers))
        assert body, path
        assets[path] = status
    initial = analyze(args.base)
    status, _, body = request(args.base, "/analyze", b'{"code":', "application/json")
    assert status == 400
    assert json.loads(body)["error"] == "The request contains invalid JSON."
    assert b"Traceback" not in body
    status, _, body = request(
        args.base, "/analyze",
        json.dumps({"lang": ".py", "code": "x" * 65537}).encode(), "application/json",
    )
    assert status == 413
    assert "64 KiB" in json.loads(body)["error"]
    assert analyze(args.base) == initial
    print(json.dumps({
        "checked_at_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "base": args.base,
        "https_certificate": "validated by urllib default trust and hostname checks",
        "homepage": 200, "assets": assets, "analysis": initial,
        "invalid_json": 400, "oversized_source": 413, "retry": 200,
    }))


if __name__ == "__main__":
    main()
