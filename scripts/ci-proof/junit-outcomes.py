import sys
import xml.etree.ElementTree as ET
from contextlib import nullcontext

report = ET.parse(sys.argv[1])
times_file = (
    open(sys.argv[2], "w", encoding="utf-8", newline="\n")
    if len(sys.argv) > 2
    else nullcontext()
)
with times_file as times:
    for case in report.iter("testcase"):
        path = (case.get("file") or "").replace("\r", "")
        if not path:
            sys.exit(f"testcase {case.get('name')} names no file")
        failed = case.find("failure") is not None or case.find("error") is not None
        status = (
            "failed"
            if failed
            else "other" if case.find("skipped") is not None else "passed"
        )
        print(f"{status}\t{path}")
        seconds = case.get("time")
        if times is not None and seconds is not None:
            print(f"{seconds}\t{path}", file=times)
