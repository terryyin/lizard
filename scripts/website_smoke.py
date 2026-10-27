"""Verify a deployed website through normal HTTPS and the real analyzer."""

import json
import time
from urllib.error import HTTPError
from urllib.parse import urljoin, urlparse
from urllib.request import Request, urlopen

SAMPLE = ("def shipping_cost(order):\n    if order.is_pickup:\n        return 0\n"
          "    if order.total >= 50:\n        return 0\n    return 5")


def wait_until_ready(url, timeout=120, interval=5):
    """Bound initial 5xx provisioning failures without retrying smoke assertions."""
    deadline = time.monotonic() + timeout
    failure = "no response"
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("Preview readiness deadline exceeded for %s: %s" % (url, failure))
        try:
            with urlopen(url.rstrip("/") + "/", timeout=min(10, remaining)) as response:
                return response.read()
        except HTTPError as error:
            if not 500 <= error.code < 600:
                raise
            failure = str(error)
            error.close()
        time.sleep(min(interval, max(0, deadline - time.monotonic())))


def verify(url):
    if urlparse(url).scheme != "https":
        raise ValueError("The preview must use normal HTTPS")

    def fetch(path, payload=None):
        request = Request(urljoin(url.rstrip("/") + "/", path), data=payload,
                          headers={"Content-Type": "application/json"}
                          if payload else {})
        with urlopen(request, timeout=30) as response:
            if response.status != 200:
                raise ValueError("Unexpected HTTP status for " + path)
            return response.read()

    if b"Lizard" not in wait_until_ready(url):
        raise ValueError("The preview homepage is not Lizard")
    manifest = json.loads(fetch("/website/static/dist/manifest.json"))
    assets = set()
    for entry in manifest.values():
        assets.add(entry["file"])
        assets.update(entry.get("css", []))
    if not assets:
        raise ValueError("The preview has no generated assets")
    for asset in sorted(assets):
        if not fetch("/website/static/dist/" + asset):
            raise ValueError("Empty generated asset: " + asset)
    result = json.loads(fetch("/analyze", json.dumps(
        {"lang": ".py", "code": SAMPLE}).encode()))
    functions = result["function_list"]
    if len(functions) != 1 or any(functions[0][key] != value for key, value in (
            ("cyclomatic_complexity", 3), ("nloc", 6), ("parameter_count", 1))):
        raise ValueError("The preview returned unexpected sample metrics")
    return result
