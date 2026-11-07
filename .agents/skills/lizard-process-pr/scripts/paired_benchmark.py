"""Paired Lizard measurements; derived from the retained cohort paired.py protocol."""

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import select
import statistics
import subprocess
import sys
import tempfile
import time


WORKER = Path(__file__).resolve().with_name("lizard_measurement.py")


def file_hash(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def source_state(root):
    files = [root / "lizard.py"]
    for name in ("lizard_core", "lizard_languages", "lizard_ext"):
        files.extend((root / name).rglob("*.py"))
    if not files[0].is_file():
        raise ValueError("missing lizard.py in " + str(root))
    return {str(p.relative_to(root)): file_hash(p) for p in sorted(files)}


def corpus_state(root):
    return {
        str(p.relative_to(root)): file_hash(p)
        for p in sorted(root.rglob("*"))
        if p.is_file()
    }


def revision(root):
    result = subprocess.run(
        ["git", "-C", str(root), "rev-parse", "HEAD"],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    return result.stdout.strip() if result.returncode == 0 else None


def evaluate(raw, limit=3.0):
    if len(raw) < 20:
        raise ValueError("at least 20 pairs required")
    for row in raw:
        if any(
            not math.isfinite(row[k]) or row[k] <= 0 for k in ("baseline", "candidate")
        ):
            raise ValueError("durations must be finite and positive")
    logs = [math.log(r["candidate"] / r["baseline"]) for r in raw]
    mean = statistics.mean(logs)
    se = statistics.stdev(logs) / math.sqrt(len(logs))
    # t(19), conservative for a two-sided 95% interval with >=20 pairs.
    interval = [math.expm1(mean + sign * 2.093 * se) * 100 for sign in (-1, 1)]
    short = any(min(r.values()) < 1.0 for r in raw)
    # Compare in log space so an exact 3% boundary cannot pass through rounding.
    passes = not short and mean + 2.093 * se < math.log1p(limit / 100)
    return {
        "pairs": len(raw),
        "paired_95pct_ci": interval,
        "medians": {
            k: statistics.median(r[k] for r in raw) for k in ("baseline", "candidate")
        },
        "stdev": {
            k: statistics.stdev(r[k] for r in raw) for k in ("baseline", "candidate")
        },
        "paired_change_pct": math.expm1(mean) * 100,
        "limit_pct": limit,
        "passes": passes,
        "verdict": (
            "inconclusive-short-batches"
            if short
            else ("passes" if passes else "bound-not-met")
        ),
    }


class Worker:
    def __init__(self, spec, directory, label, log):
        path = directory / (label + ".json")
        path.write_text(json.dumps(spec))
        self.process = subprocess.Popen(
            [sys.executable, "-I", str(WORKER), str(path)],
            cwd=directory,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=log,
            bufsize=0,
        )
        self.buffer = b""

    def receive(self):
        deadline = time.monotonic() + 120
        while b"\n" not in self.buffer:
            remaining = deadline - time.monotonic()
            if (
                remaining <= 0
                or not select.select([self.process.stdout], [], [], remaining)[0]
            ):
                raise ValueError("worker timeout; inspect retained stderr")
            part = os.read(self.process.stdout.fileno(), 65536)
            if not part:
                raise ValueError("worker exited; inspect retained stderr")
            self.buffer += part
        line, self.buffer = self.buffer.split(b"\n", 1)
        return json.loads(line)

    def request(self, loops):
        self.process.stdin.write((str(loops) + "\n").encode())
        return self.receive()

    def measure(self, loops):
        value = self.request(loops)["seconds"]
        if not math.isfinite(value) or value <= 0:
            raise ValueError("invalid worker duration")
        return value

    def close(self):
        if self.process.poll() is None:
            self.process.terminate()
        try:
            self.process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait()
        self.process.stdin.close()
        self.process.stdout.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("baseline", "candidate", "corpus", "out"):
        parser.add_argument("--" + name, required=True, type=Path)
    parser.add_argument(
        "--workload", choices=("api", "api-parameters", "cli"), required=True
    )
    parser.add_argument("--extension", help="extension name, as passed to Lizard -E")
    parser.add_argument("--pairs", type=int, default=20)
    parser.add_argument("--seconds", type=float, default=1.2)
    parser.add_argument("--limit-pct", type=float, default=3.0)
    args = parser.parse_args()
    if (
        args.pairs < 20
        or not math.isfinite(args.seconds)
        or not 1 <= args.seconds <= 30
    ):
        parser.error(
            "require >=20 pairs and a calibration target between 1 and 30 seconds"
        )
    if not math.isfinite(args.limit_pct) or args.limit_pct <= 0:
        parser.error("limit must be finite and positive")
    args.out, args.corpus = args.out.resolve(), args.corpus.resolve()
    if args.corpus == args.out or args.corpus in args.out.parents:
        parser.error("output must be outside the frozen corpus")
    args.out.parent.mkdir(parents=True, exist_ok=True)
    workers, logs = [], []
    report = {
        "command": sys.argv,
        "python": sys.version,
        "executable": sys.executable,
        "host": os.uname().nodename,
        "load_before": os.getloadavg(),
        "workload": args.workload,
        "extension": args.extension,
        "raw": [],
        "passes": False,
        "verdict": "setup-error",
    }
    try:
        output = args.out.open("x")
    except FileExistsError:
        print(
            json.dumps(
                {"passes": False, "error": "output exists; choose a new evidence path"}
            )
        )
        return 2
    try:
        roots = [args.baseline.resolve(), args.candidate.resolve()]
        report["sources"] = [
            {"path": str(p), "revision": revision(p), "files": source_state(p)}
            for p in roots
        ]
        report["corpus"] = {
            "path": str(args.corpus),
            "files": corpus_state(args.corpus),
        }
        report["harness"] = {
            str(p): file_hash(p) for p in (Path(__file__).resolve(), WORKER)
        }
        if not report["corpus"]["files"]:
            raise ValueError("empty corpus")
        with tempfile.TemporaryDirectory(prefix="lizard-paired-") as temporary:
            directory = Path(temporary)
            for label, root in zip(("baseline", "candidate"), roots):
                log = args.out.with_name(
                    args.out.name + "." + label + ".stderr.log"
                ).open("x")
                logs.append(log)
                spec = {
                    "checkout": str(root),
                    "corpus": str(args.corpus),
                    "workload": args.workload,
                    "extension": args.extension,
                }
                worker = Worker(spec, directory, label, log)
                workers.append(worker)
                worker.ready = worker.receive()
            report["workers"] = [w.ready for w in workers]
            for w in workers:
                w.measure(3)
            loops = 1
            calibration = []
            report["calibration"] = calibration
            for _ in range(8):
                durations = [w.measure(loops) for w in workers]
                calibration.append({"loops": loops, "seconds": durations})
                if min(durations) >= args.seconds:
                    break
                loops = max(loops + 1, math.ceil(loops * args.seconds / min(durations)))
            else:
                raise ValueError("could not calibrate stable batch duration")
            report["loops"] = loops
            for w in workers:
                w.measure(loops)
            for pair in range(args.pairs):
                row = {}
                report["raw"].append(row)
                for which in (0, 1) if pair % 2 == 0 else (1, 0):
                    row[("baseline", "candidate")[which]] = workers[which].measure(
                        loops
                    )
            report["stability"] = [w.request(0) for w in workers]
        if any(source_state(p) != s["files"] for p, s in zip(roots, report["sources"])):
            raise ValueError("production source changed during measurement")
        if corpus_state(args.corpus) != report["corpus"]["files"]:
            raise ValueError("corpus changed during measurement")
        if any(
            file_hash(Path(p)) != expected for p, expected in report["harness"].items()
        ):
            raise ValueError("harness changed during measurement")
        report.update(evaluate(report["raw"], args.limit_pct))
    except (ValueError, KeyError, OSError, subprocess.SubprocessError) as error:
        report.update(
            passes=False, verdict="setup-or-measurement-error", error=str(error)
        )
    finally:
        for worker in workers:
            if hasattr(worker, "process"):
                worker.close()
        for log in logs:
            log.close()
        report["load_after"] = os.getloadavg()
        json.dump(report, output, indent=2)
        output.write("\n")
        output.close()
    summary = {
        key: report[key]
        for key in ("verdict", "passes", "paired_95pct_ci", "error")
        if key in report
    }
    summary["out"] = str(args.out)
    print(json.dumps(summary))
    return 0 if report["passes"] else 1


if __name__ == "__main__":
    sys.exit(main())
