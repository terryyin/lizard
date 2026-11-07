"""Isolated persistent worker for paired_benchmark.py; stdout is a JSON protocol."""

import hashlib
import importlib
import json
from pathlib import Path
import subprocess
import sys
import time


def observation(result):
    return {
        "filename": result.filename,
        "functions": [
            {
                key: getattr(function, key)
                for key in (
                    "name",
                    "long_name",
                    "start_line",
                    "end_line",
                    "nloc",
                    "cyclomatic_complexity",
                    "token_count",
                    "parameters",
                    "parameter_count",
                )
            }
            for function in result.function_list
        ],
    }


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def run_cli(command, capture=True):
    result = subprocess.run(
        command,
        stdout=subprocess.PIPE if capture else subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )
    if (
        result.returncode not in (0, 1)
        or b"Traceback (most recent call last):" in result.stderr
    ):
        raise ValueError("CLI failed: " + result.stderr.decode(errors="replace")[:2000])
    if not capture:
        return None
    return {
        "returncode": result.returncode,
        "stdout_sha256": hashlib.sha256(result.stdout).hexdigest(),
        "stderr_sha256": hashlib.sha256(result.stderr).hexdigest(),
    }


def module_paths(checkout):
    paths = {}
    for name, module in list(sys.modules.items()):
        if name == "lizard" or name.startswith(
            ("lizard_core", "lizard_languages", "lizard_ext")
        ):
            filename = getattr(module, "__file__", None)
            if filename:
                path = Path(filename).resolve()
                if checkout not in path.parents:
                    raise ValueError("foreign module: " + name + " at " + str(path))
                paths[name] = str(path)
    return paths


def run(spec):
    checkout = Path(spec["checkout"]).resolve()
    corpus = Path(spec["corpus"]).resolve()
    sys.path.insert(0, str(checkout))
    lizard = importlib.import_module("lizard")
    extensions = []
    if spec["extension"]:
        extensions = lizard.get_extensions([spec["extension"]])
    analyzer = lizard.FileAnalyzer(extensions) if extensions else lizard.analyze_file
    cases = [
        (str(p), p.read_text(encoding="utf-8"))
        for p in sorted(corpus.rglob("*"))
        if p.is_file()
    ]
    if not cases:
        raise ValueError("empty corpus")
    command = [sys.executable, "-I", str(checkout / "lizard.py")]
    if spec["extension"]:
        command += ["-E" + spec["extension"]]
    command += [str(corpus)]

    def observe():
        if spec["workload"] == "cli":
            return run_cli(command)
        return [
            observation(analyzer.analyze_source_code(name, source))
            for name, source in cases
        ]

    before = digest(observe())
    initial_paths = module_paths(checkout)
    print(
        json.dumps(
            {
                "ready": True,
                "observation_sha256": before,
                "modules": initial_paths,
                "cli_command": command,
                "files": len(cases),
            }
        ),
        flush=True,
    )
    for line in sys.stdin:
        loops = int(line)
        if loops == 0:
            after = digest(observe())
            paths = module_paths(checkout)
            if before != after or paths != initial_paths:
                raise ValueError(
                    "public observations or module paths changed during measurement"
                )
            print(json.dumps({"stable": True, "observation_sha256": after}), flush=True)
            return
        if loops < 0:
            raise ValueError("loops must be positive")
        start = time.perf_counter()
        for _ in range(loops):
            if spec["workload"] == "cli":
                run_cli(command, capture=False)
            else:
                for name, source in cases:
                    result = analyzer.analyze_source_code(name, source)
                    if spec["workload"] == "api-parameters":
                        for function in result.function_list:
                            function.parameters
                            function.parameter_count
        print(json.dumps({"seconds": time.perf_counter() - start}), flush=True)


if __name__ == "__main__":
    run(json.loads(Path(sys.argv[1]).read_text()))
