#!/usr/bin/env python3
"""Compare per-script outcomes from two xr2f_batch result files."""

from __future__ import annotations

import argparse
from collections import Counter
from dataclasses import dataclass
from datetime import datetime
import ntpath
from pathlib import Path
import re
import sys


STARTED_RE = re.compile(
    r"^Started:\s*(?P<value>\d{4}-\d{2}-\d{2}\s+\d{1,2}:\d{2}:\d{2}\s+[AP]M)\s*$",
    re.MULTILINE,
)
SUMMARY_ROW_RE = re.compile(
    r"^(?P<source>.+?\.[Rr])\s{2,}(?P<status>PASS|FAIL|SKIP)\s+"
    r"(?P<outcome>[a-z][a-z0-9_]*)\b",
    re.MULTILINE,
)
OUTCOME_PROGRESS = {
    "transpile_fail": 0,
    "compile_fail": 1,
    "run_fail": 2,
    "full_pass": 3,
}


@dataclass(frozen=True)
class RunReport:
    path: Path
    started: datetime | None
    outcomes: dict[str, tuple[str, str]]


def normalized_script_path(path: str) -> str:
    """Return a case-insensitive Windows path key without requiring Windows."""
    return ntpath.normcase(ntpath.normpath(path.strip()))


def parse_report(path: Path) -> RunReport:
    text = path.read_text(encoding="utf-8-sig", errors="replace")
    started_match = STARTED_RE.search(text)
    started = (
        datetime.strptime(started_match.group("value"), "%Y-%m-%d %I:%M:%S %p")
        if started_match
        else None
    )

    outcomes: dict[str, tuple[str, str]] = {}
    for match in SUMMARY_ROW_RE.finditer(text):
        source = match.group("source").strip()
        key = normalized_script_path(source)
        value = (source, match.group("outcome"))
        previous = outcomes.get(key)
        if previous is not None and previous != value:
            raise ValueError(f"conflicting summary rows for {source!r} in {path}")
        outcomes[key] = value

    if not outcomes:
        raise ValueError(f"no R-script summary rows found in {path}")
    return RunReport(path=path, started=started, outcomes=outcomes)


def chronological_reports(first: RunReport, second: RunReport) -> tuple[RunReport, RunReport]:
    if first.started is not None and second.started is not None:
        return (first, second) if first.started <= second.started else (second, first)
    return first, second


def format_started(report: RunReport) -> str:
    if report.started is None:
        return "timestamp unavailable"
    return report.started.strftime("%Y-%m-%d %I:%M:%S %p")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "List R scripts whose xr2f_batch outcome changed between two result files. "
            "Files are ordered using their Started timestamps when both are available."
        )
    )
    parser.add_argument("result_file_1", type=Path)
    parser.add_argument("result_file_2", type=Path)
    parser.add_argument(
        "--worse-only",
        "--regressions-only",
        action="store_true",
        help=(
            "show only support regressions where a previously full-pass script now "
            "fails during transpilation, compilation, or execution"
        ),
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        first = parse_report(args.result_file_1)
        second = parse_report(args.result_file_2)
    except (OSError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    older, newer = chronological_reports(first, second)
    common = older.outcomes.keys() & newer.outcomes.keys()
    changes = [
        (
            newer.outcomes[key][0],
            older.outcomes[key][1],
            newer.outcomes[key][1],
        )
        for key in common
        if older.outcomes[key][1] != newer.outcomes[key][1]
    ]
    changes.sort(key=lambda item: normalized_script_path(item[0]))
    if args.worse_only:
        changes = [
            change
            for change in changes
            if change[1] == "full_pass"
            and change[2] in OUTCOME_PROGRESS
            and change[2] != "full_pass"
        ]

    print(f"Older: {older.path} ({format_started(older)})")
    print(f"Newer: {newer.path} ({format_started(newer)})")
    print(f"Common scripts: {len(common)}")
    label = "Worse outcomes" if args.worse_only else "Changed outcomes"
    print(f"{label}: {len(changes)}")

    transition_counts = Counter((old, new) for _, old, new in changes)
    if transition_counts:
        print("Transitions:")
        for (old, new), count in sorted(transition_counts.items()):
            print(f"  {old} -> {new}: {count}")
        print("Scripts:")
        for source, old, new in changes:
            print(f"  {source}: {old} -> {new}")

    only_older = len(older.outcomes.keys() - newer.outcomes.keys())
    only_newer = len(newer.outcomes.keys() - older.outcomes.keys())
    if only_older or only_newer:
        print(f"Unmatched scripts: older_only={only_older} newer_only={only_newer}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
