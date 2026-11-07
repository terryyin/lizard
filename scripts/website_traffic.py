"""Preserve App Engine traffic allocations and their sharding method."""

import math
import shlex


def traffic_split(service):
    split = service.get("split")
    allocations = split.get("allocations") if isinstance(split, dict) else None
    if (not isinstance(allocations, dict) or not allocations or any(
            isinstance(value, bool) or not isinstance(value, (int, float))
            or not 0 <= value <= 1 for value in allocations.values())
            or not math.isclose(sum(allocations.values()), 1, abs_tol=1e-8)):
        raise ValueError("Cannot establish the current traffic split; inspect the service before retrying")
    split_mode(split)
    return split


def split_mode(split):
    mode = split.get("shardBy", "IP")
    if mode == "UNSPECIFIED":
        mode = "IP"
    if mode not in ("IP", "COOKIE", "RANDOM"):
        raise ValueError("Cannot establish traffic sharding; inspect the service before retrying")
    return mode.lower()


def same_split(left, right):
    return (left["allocations"] == right["allocations"]
            and split_mode(left) == split_mode(right))


def traffic_arguments(split):
    weights = ",".join("%s=%s" % (version, share)
                       for version, share in sorted(split["allocations"].items()))
    return ["--splits=" + weights, "--split-by=" + split_mode(split)]


def recovery_command(project, service, split):
    return shlex.join(["gcloud", "app", "services", "set-traffic", service,
                       "--project=" + project] + traffic_arguments(split))
