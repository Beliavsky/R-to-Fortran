from __future__ import annotations

import importlib.util
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
REPAIR_PATH = REPO_ROOT / "xr2f_repair.py"


def _load_repair_module():
    spec = importlib.util.spec_from_file_location("xr2f_repair_for_test", REPAIR_PATH)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_xr2f_repair_extracts_unique_directive_candidates() -> None:
    repair = _load_repair_module()
    text = """
    Concrete candidates:
    - `# xr2f: force_rank(f.y)=1`
    - `# xr2f: force_kind(foo.bar)=real`
    - `# xr2f: force_rank(f.y)=1`
    """

    assert repair.extract_directive_candidates(text) == [
        "# xr2f: force_rank(f.y)=1",
        "# xr2f: force_kind(foo.bar)=real",
    ]


def test_xr2f_repair_prepends_directives_after_shebang() -> None:
    repair = _load_repair_module()
    source = "#!/usr/bin/env Rscript\nx <- 1\n"

    out = repair.prepend_directives(source, ["# xr2f: force_rank(f.y)=1"])

    assert out.startswith("#!/usr/bin/env Rscript\n# xr2f repair trial directives\n")
    assert "# xr2f: force_rank(f.y)=1\n\nx <- 1\n" in out


def test_xr2f_repair_resolves_source_from_bundle(tmp_path: Path) -> None:
    repair = _load_repair_module()
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "foo_directed.r").write_text("# xr2f: force_rank(x)=1\n", encoding="utf-8")
    source = bundle / "foo.r"
    source.write_text("x <- 1\n", encoding="utf-8")

    assert repair.resolve_source_from_bundle(bundle) == source


def test_xr2f_repair_classifies_compile_and_diff_status() -> None:
    repair = _load_repair_module()

    assert repair.classify_output(0, "Build: PASS\n", "", "") == ("compile_pass", 30)
    assert repair.classify_output(0, "Run: PASS\n", "", "") == ("run_pass", 40)
    assert repair.classify_output(0, "", "", "Run diff: MATCH\n") == ("run_diff_match", 50)
    assert repair.classify_output(1, "Build: FAIL\nError: bad\n", "", "") == ("compile_fail", 10)
