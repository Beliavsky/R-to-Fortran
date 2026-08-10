from pathlib import Path
import subprocess
import sys


SCRIPT = Path(__file__).resolve().parents[1] / "compare_xr2f_batch_results.py"


def _report(started: str, rows: list[tuple[str, str, str]]) -> str:
    lines = [f"Started: {started}", "", "Summary:", "source  status  outcome  R_loc  Fortran_loc"]
    for source, status, outcome in rows:
        lines.append(f"C:\\rosetta\\{source}  {status}    {outcome}  1  1")
    lines.extend([f"Totals: {len(rows)} files", "Outcomes: full_pass=0"])
    return "\n".join(lines) + "\n"


def test_compare_xr2f_batch_results_orders_runs_and_lists_outcome_changes(tmp_path: Path) -> None:
    newer = tmp_path / "newer.txt"
    older = tmp_path / "older.txt"
    older.write_text(
        _report(
            "2026-07-27 08:00:00 PM",
            [
                ("Alpha.R", "FAIL", "transpile_fail"),
                ("Beta.R", "FAIL", "compile_fail"),
                ("Delta.R", "FAIL", "compile_fail"),
                ("Gamma.R", "PASS", "full_pass"),
                ("Same.R", "PASS", "full_pass"),
            ],
        ),
        encoding="utf-8",
    )
    newer.write_text(
        _report(
            "2026-07-28 08:00:00 AM",
            [
                ("Alpha.R", "PASS", "full_pass"),
                ("Beta.R", "FAIL", "run_fail"),
                ("Delta.R", "FAIL", "transpile_fail"),
                ("Gamma.R", "FAIL", "compile_fail"),
                ("Same.R", "PASS", "full_pass"),
            ],
        ),
        encoding="utf-8",
    )

    proc = subprocess.run(
        [sys.executable, str(SCRIPT), str(newer), str(older)],
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert f"Older: {older}" in proc.stdout
    assert f"Newer: {newer}" in proc.stdout
    assert "Common scripts: 5" in proc.stdout
    assert "Changed outcomes: 4" in proc.stdout
    assert "transpile_fail -> full_pass: 1" in proc.stdout
    assert "compile_fail -> run_fail: 1" in proc.stdout
    assert r"C:\rosetta\Alpha.R: transpile_fail -> full_pass" in proc.stdout
    assert "Same.R" not in proc.stdout

    worse_proc = subprocess.run(
        [sys.executable, str(SCRIPT), "--worse-only", str(newer), str(older)],
        capture_output=True,
        text=True,
        check=False,
    )

    assert worse_proc.returncode == 0, worse_proc.stdout + worse_proc.stderr
    assert "Worse outcomes: 1" in worse_proc.stdout
    assert "full_pass -> compile_fail: 1" in worse_proc.stdout
    assert r"C:\rosetta\Gamma.R: full_pass -> compile_fail" in worse_proc.stdout
    assert "Alpha.R" not in worse_proc.stdout
    assert "Beta.R" not in worse_proc.stdout
    assert "Delta.R" not in worse_proc.stdout
