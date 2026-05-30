from __future__ import annotations

import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
XR2F_BATCH_PATH = REPO_ROOT / "xr2f_batch.py"
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))


def test_expand_inputs_supports_at_list_files_and_skip_lines(tmp_path: Path, monkeypatch) -> None:
    src_dir = tmp_path / "src"
    src_dir.mkdir()
    skipped = src_dir / "skip.r"
    a_r = src_dir / "a.r"
    b_r = src_dir / "b.R"
    skip_txt = src_dir / "skip.txt"
    skipped.write_text("print(0)\n", encoding="utf-8")
    a_r.write_text("print(1)\n", encoding="utf-8")
    b_r.write_text("print(2)\n", encoding="utf-8")
    skip_txt.write_text("not R\n", encoding="utf-8")

    nested_list = tmp_path / "nested_list.txt"
    nested_list.write_text("src/b.R\n", encoding="utf-8")

    file_list = tmp_path / "file_list.txt"
    file_list.write_text(
        "\n".join(
            [
                "src/skip.r",
                "# comment",
                "src/a.r",
                "src/skip.txt",
                "@nested_list.txt",
                "",
            ]
        ),
        encoding="utf-8",
    )

    monkeypatch.chdir(tmp_path)
    import xr2f_batch

    expanded, errors = xr2f_batch._expand_inputs(["@file_list.txt"], skip_lines=2)

    assert errors == []
    assert expanded == [a_r, b_r]


def test_xr2f_batch_reports_missing_at_file(tmp_path: Path) -> None:
    proc = subprocess.run(
        [sys.executable, str(XR2F_BATCH_PATH), "@missing.txt"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 1
    assert "@ file not found:" in proc.stdout


def test_xr2f_batch_self_contained_limit_and_tee(tmp_path: Path) -> None:
    a_r = tmp_path / "a.r"
    b_r = tmp_path / "b.r"
    a_r.write_text("print(1 + 2)\n", encoding="utf-8")
    b_r.write_text("print(3 + 4)\n", encoding="utf-8")
    file_list = tmp_path / "list.txt"
    file_list.write_text(f"{a_r}\n{b_r}\n", encoding="utf-8")
    tee_path = tmp_path / "batch_results.txt"

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_BATCH_PATH),
            f"@{file_list}",
            "--self-contained",
            "--limit",
            "1",
            "--tee",
            str(tee_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Totals: 1 files, 1 pass, 0 fail" in proc.stdout
    text = tee_path.read_text(encoding="utf-8")
    assert "Command:" in text
    assert "--self-contained" in text
    assert "Totals: 1 files, 1 pass, 0 fail" in text


def test_xr2f_batch_max_fail_alias_stops(tmp_path: Path) -> None:
    bad1 = tmp_path / "bad1.r"
    bad2 = tmp_path / "bad2.r"
    bad1.write_text("library(no_such_package)\n", encoding="utf-8")
    bad2.write_text("library(no_such_package)\n", encoding="utf-8")

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_BATCH_PATH),
            str(bad1),
            str(bad2),
            "--maxfail",
            "1",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 1
    assert "Stopped at max-fail=1." in proc.stdout
    assert "Totals: 1 files, 0 pass, 1 fail" in proc.stdout
