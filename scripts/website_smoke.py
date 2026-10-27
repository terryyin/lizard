"""Verify a deployed website through normal HTTPS and the real analyzer."""

import json
from urllib.parse import urljoin, urlparse
from urllib.request import Request, urlopen

SAMPLE = ("def shipping_cost(order):\n    if order.is_pickup:\n        return 0\n"
          "    if order.total >= 50:\n        return 0\n    return 5")


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

    if b"Lizard" not in fetch("/"):
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
