#!/usr/bin/env python3
"""Small IDE for xr2f.py: edit R, view generated generic Fortran, run both."""

from __future__ import annotations

import argparse
import hashlib
import re
import shlex
import subprocess
import sys
import tempfile
import time
import tkinter as tk
from dataclasses import dataclass
from pathlib import Path
from tkinter import filedialog, messagebox, ttk


ROOT = Path(__file__).resolve().parent
DEFAULT_XR2F = ROOT / "xr2f.py"
DEFAULT_R_HELPER = ROOT / "r.f90"

COMPILER_MODES = [
    "gfortran",
    "gfortran -O2",
    "gfortran -O3",
    "ifx",
    "ifx /O2",
    "lfortran",
]

R_FILETYPES = [("R files", "*.R *.r"), ("All files", "*.*")]
FORTRAN_FILETYPES = [("Fortran files", "*.f90 *.f95 *.f03 *.f08"), ("All files", "*.*")]

R_KEYWORDS = {
    "break", "else", "FALSE", "for", "function", "if", "in", "NA", "NaN",
    "next", "NULL", "repeat", "return", "TRUE", "while",
}
R_BUILTINS = {
    "abs", "all", "any", "as.numeric", "c", "cat", "cbind", "chol",
    "crossprod", "data.frame", "diag", "dim", "exp", "length", "list",
    "log", "matrix", "max", "mean", "min", "ncol", "nrow", "print",
    "rbind", "rep", "seq", "seq_len", "sqrt", "sum", "t", "var",
}
FORTRAN_KEYWORDS = {
    "allocatable", "allocate", "call", "contains", "cycle", "deallocate",
    "do", "elemental", "else", "end", "function", "if", "implicit",
    "integer", "interface", "intrinsic", "logical", "module", "none",
    "only", "parameter", "program", "pure", "real", "result", "return",
    "subroutine", "then", "type", "use",
}

STRING_RE = re.compile(r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'')
NUMBER_RE = re.compile(r"\b\d+(?:\.\d*)?(?:[eE][+-]?\d+)?\b|\.\d+(?:[eE][+-]?\d+)?\b")
R_COMMENT_RE = re.compile(r"#[^\n]*")
FORTRAN_COMMENT_RE = re.compile(r"![^\n]*")
FLOAT_TOKEN_RE = re.compile(
    r"(?<![\w.])([+-]?(?:(?:\d+\.\d*|\.\d+)(?:[eEdD][+-]?\d+)?|\d+[eEdD][+-]?\d+))(?![\w.])"
)


@dataclass
class TranslateResult:
    ok: bool
    fortran: str
    stdout: str
    stderr: str
    elapsed: float
    command: list[str]


@dataclass
class RunResult:
    ok: bool
    stdout: str
    stderr: str
    elapsed: float
    command: list[str]
    compile_elapsed: float | None = None
    run_elapsed: float | None = None


def word_re(words: set[str]) -> re.Pattern[str]:
    return re.compile(r"\b(" + "|".join(sorted(map(re.escape, words))) + r")\b", re.IGNORECASE)


R_KEYWORD_RE = word_re(R_KEYWORDS)
R_BUILTIN_RE = word_re(R_BUILTINS)
FORTRAN_KEYWORD_RE = word_re(FORTRAN_KEYWORDS)


def read_text_normalized(path: Path) -> str:
    text = path.read_bytes().decode("utf-8-sig", errors="replace")
    return text.replace("\r\r\n", "\n").replace("\r\n", "\n").replace("\r", "\n")


def display_fortran(fortran: str) -> str:
    return re.sub(r"\A! transpiled by xr2f\.py from .+? on .+?\n", "", fortran, count=1)


def strip_r_comment(line: str) -> str:
    quote = ""
    escape = False
    for i, ch in enumerate(line):
        if quote:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = ""
            continue
        if ch in {"'", '"'}:
            quote = ch
        elif ch == "#":
            return line[:i]
    return line


def r_text_has_open_string(line: str) -> bool:
    quote = ""
    escape = False
    for ch in line:
        if quote:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = ""
            continue
        if ch in {"'", '"'}:
            quote = ch
        elif ch == "#":
            break
    return bool(quote)


def leading_whitespace(text: str) -> str:
    return text[: len(text) - len(text.lstrip(" \t"))]


def r_source_waiting_for_completion(source: str, *, require_enter: bool) -> bool:
    if not source or source.endswith(("\n", "\r")):
        return False
    line = source.splitlines()[-1]
    code = R_COMMENT_RE.sub("", line).rstrip()
    if not code:
        return False
    if require_enter:
        return True
    if re.search(r"(\^|\+|-|\*|/|%%|%/%|,|<-|=|\(|\[|\{)\s*$", code):
        return True
    stack: list[str] = []
    pairs = {"(": ")", "[": "]", "{": "}"}
    quote = ""
    escape = False
    for ch in source:
        if quote:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = ""
            continue
        if ch in {"'", '"'}:
            quote = ch
        elif ch in pairs:
            stack.append(pairs[ch])
        elif ch in {")", "]", "}"} and stack and stack[-1] == ch:
            stack.pop()
    return bool(quote or stack)


def selected_text_lines(widget: tk.Text) -> list[int]:
    try:
        start = widget.index("sel.first")
        end = widget.index("sel.last")
    except tk.TclError:
        return []
    start_line = int(start.split(".", 1)[0])
    end_line, end_col = (int(part) for part in end.split(".", 1))
    if end_col == 0 and end_line > start_line:
        end_line -= 1
    return list(range(start_line, end_line + 1))


def r_name_to_fortran(name: str) -> str:
    return name.replace(".", "_").lower()


def r_mapping_keys(line: str) -> list[str]:
    code = R_COMMENT_RE.sub("", line).strip()
    if not code:
        return []
    match = re.match(r"([A-Za-z.]\w*)\s*(?:<-|=(?!=))", code)
    if match:
        keys = [f"assign:{r_name_to_fortran(match.group(1))}"]
        if re.search(r"\bfunction\s*\(", code):
            keys.append(f"proc:{r_name_to_fortran(match.group(1))}")
        return keys
    match = re.match(r"for\s*\(\s*([A-Za-z.]\w*)\s+in\b", code)
    if match:
        return [f"do:{r_name_to_fortran(match.group(1))}"]
    if re.match(r"if\s*\(", code):
        return ["if"]
    if re.match(r"while\s*\(", code):
        return ["while"]
    if re.match(r"(?:print|cat)\s*\(", code):
        return ["print"]
    return []


def fortran_mapping_keys(line: str) -> list[str]:
    stripped = line.strip()
    lower = stripped.lower()
    if not stripped or lower.startswith(("!", "use ", "implicit ", "end ")):
        return []
    match = re.match(r"([A-Za-z_]\w*)\s*=", lower)
    if match:
        return [f"assign:{match.group(1)}"]
    match = re.match(r"do\s+([A-Za-z_]\w*)\s*=", lower)
    if match:
        return [f"do:{match.group(1)}"]
    if lower.startswith("if ") or lower.startswith("if("):
        return ["if"]
    if lower.startswith(("print ", "write(", "call print_")):
        return ["print"]
    match = re.match(
        r"(?:(?:pure|impure|elemental|recursive)\s+)*"
        r"(?:(?:real|integer|logical|character|complex|type)\b(?:\([^)]*\))?\s+)?"
        r"(?:function|subroutine)\s+([A-Za-z_]\w*)",
        lower,
    )
    if match:
        return [f"proc:{match.group(1)}"]
    return []


def build_source_fortran_map(source_lines: list[str], fortran: str) -> dict[int, set[int]]:
    fortran_keys: dict[str, list[int]] = {}
    for line_no, line in enumerate(fortran.splitlines(), 1):
        for key in fortran_mapping_keys(line):
            fortran_keys.setdefault(key, []).append(line_no)
    mapping: dict[int, set[int]] = {}
    used_by_key: dict[str, int] = {}
    for source_no, line in enumerate(source_lines, 1):
        for key in r_mapping_keys(line):
            matches = fortran_keys.get(key, [])
            index = used_by_key.get(key, 0)
            if index < len(matches):
                mapping.setdefault(source_no, set()).add(matches[index])
                used_by_key[key] = index + 1
    return mapping


def format_float_tokens(text: str, decimals: int) -> str:
    def repl(match: re.Match[str]) -> str:
        token = match.group(1)
        try:
            value = float(token.replace("D", "E").replace("d", "e"))
        except ValueError:
            return token
        if "e" in token.lower() or "d" in token.lower():
            return f"{value:.{decimals}e}"
        return f"{value:.{decimals}f}"

    return FLOAT_TOKEN_RE.sub(repl, text)


def translate_r_to_fortran(
    source: str,
    *,
    xr2f: Path,
    source_name: str,
    pretty: bool,
    timeout: float | None,
) -> TranslateResult:
    start = time.perf_counter()
    with tempfile.TemporaryDirectory(prefix="xr2f_ide_translate_") as tmp:
        tmpdir = Path(tmp)
        r_path = tmpdir / source_name
        f_path = tmpdir / "xr2f_ide_session.f90"
        r_path.write_text(source, encoding="utf-8")
        cmd = [sys.executable, str(xr2f), str(r_path), "--out", str(f_path)]
        if pretty:
            cmd.append("--pretty")
        try:
            cp = subprocess.run(cmd, text=True, capture_output=True, timeout=timeout)
        except subprocess.TimeoutExpired as exc:
            elapsed = time.perf_counter() - start
            return TranslateResult(False, "", exc.stdout or "", f"xr2f timed out after {exc.timeout} seconds", elapsed, cmd)
        elapsed = time.perf_counter() - start
        fortran = f_path.read_text(encoding="utf-8", errors="replace") if f_path.exists() else ""
        return TranslateResult(cp.returncode == 0 and bool(fortran), fortran, cp.stdout or "", cp.stderr or "", elapsed, cmd)


def run_r_source(
    source: str,
    *,
    rscript: str,
    source_name: str,
    run_dir: Path,
    timeout: float | None,
) -> RunResult:
    start = time.perf_counter()
    with tempfile.TemporaryDirectory(prefix="xr2f_ide_r_run_") as tmp:
        r_path = Path(tmp) / source_name
        r_path.write_text(source, encoding="utf-8")
        cmd = [*shlex.split(rscript), str(r_path)]
        try:
            cp = subprocess.run(cmd, cwd=run_dir, text=True, capture_output=True, timeout=timeout)
        except FileNotFoundError:
            elapsed = time.perf_counter() - start
            return RunResult(False, "", f"{rscript!r} was not found on PATH", elapsed, cmd)
        except subprocess.TimeoutExpired as exc:
            elapsed = time.perf_counter() - start
            return RunResult(False, exc.stdout or "", f"R timed out after {exc.timeout} seconds", elapsed, cmd)
        elapsed = time.perf_counter() - start
        return RunResult(cp.returncode == 0, cp.stdout or "", cp.stderr or "", elapsed, cmd)


def helper_paths_for_fortran(fortran: str) -> list[Path]:
    if re.search(r"(?im)^\s*use\s+r_mod\b", fortran) and DEFAULT_R_HELPER.exists():
        return [DEFAULT_R_HELPER]
    return []


def gfortran_options_for_mode(mode: str) -> list[str]:
    if mode == "gfortran -O2":
        return ["-O2"]
    if mode == "gfortran -O3":
        return ["-O3"]
    return []


def cached_r_helper_for_gfortran(mode: str, run_dir: Path, timeout: float | None) -> tuple[Path, Path, str, float]:
    helper = DEFAULT_R_HELPER.resolve()
    helper_bytes = helper.read_bytes()
    options = [*gfortran_options_for_mode(mode), "-cpp"]
    key_data = b"\0".join(
        [
            str(helper).encode("utf-8"),
            mode.encode("utf-8"),
            " ".join(options).encode("utf-8"),
            sys.platform.encode("utf-8"),
            helper_bytes,
        ]
    )
    key = hashlib.sha256(key_data).hexdigest()[:24]
    cache_dir = Path(tempfile.gettempdir()) / "xr2f_ide_runtime_cache" / key
    cache_dir.mkdir(parents=True, exist_ok=True)
    obj = cache_dir / "r.o"
    mod = cache_dir / "r_mod.mod"
    if obj.exists() and mod.exists():
        return obj, cache_dir, "", 0.0
    cmd = ["gfortran", *options, "-c", str(helper), "-J", str(cache_dir), "-I", str(cache_dir), "-o", str(obj)]
    start = time.perf_counter()
    cp = subprocess.run(cmd, cwd=run_dir, text=True, capture_output=True, timeout=timeout)
    elapsed = time.perf_counter() - start
    if cp.returncode != 0:
        raise RuntimeError("\n".join(part.rstrip() for part in (cp.stdout, cp.stderr) if part and part.strip()))
    return obj, cache_dir, "", elapsed


def _uses_r_helper(sources: list[Path]) -> bool:
    return any(src.name.lower() == "r.f90" for src in sources)


def ifx_command(sources: list[Path], exe: Path, options: list[str]) -> list[str]:
    options = list(options)
    if _uses_r_helper(sources):
        fpp = "/fpp" if sys.platform.startswith("win") else "-fpp"
        if fpp not in options:
            options.insert(0, fpp)
    if sys.platform.startswith("win"):
        return ["ifx", *options, *map(str, sources), f"/Fe:{exe}"]
    return ["ifx", *options, *map(str, sources), "-o", str(exe)]


def compiler_command(mode: str, sources: list[Path], exe: Path, *, include_dirs: list[Path] | None = None) -> list[str]:
    include_dirs = include_dirs or []
    include_flags = [flag for inc in include_dirs for flag in ("-I", str(inc))]
    gfortran_cpp = ["-cpp"] if _uses_r_helper(sources) else []
    if mode == "gfortran":
        return ["gfortran", *gfortran_cpp, *include_flags, *map(str, sources), "-o", str(exe)]
    if mode == "gfortran -O2":
        return ["gfortran", "-O2", *gfortran_cpp, *include_flags, *map(str, sources), "-o", str(exe)]
    if mode == "gfortran -O3":
        return ["gfortran", "-O3", *gfortran_cpp, *include_flags, *map(str, sources), "-o", str(exe)]
    if mode == "ifx":
        return ifx_command(sources, exe, [])
    if mode == "ifx /O2":
        return ifx_command(sources, exe, ["/O2"] if sys.platform.startswith("win") else ["-O2"])
    if mode == "lfortran":
        return ["lfortran", *map(str, sources), "-o", str(exe)]
    return ["gfortran", *gfortran_cpp, *include_flags, *map(str, sources), "-o", str(exe)]


def run_fortran_source(fortran: str, *, mode: str, run_dir: Path, timeout: float | None) -> RunResult:
    start = time.perf_counter()
    with tempfile.TemporaryDirectory(prefix="xr2f_ide_f_run_") as tmp:
        tmpdir = Path(tmp)
        source = tmpdir / "xr2f_ide_session.f90"
        exe = tmpdir / ("xr2f_ide_session.exe" if sys.platform.startswith("win") else "xr2f_ide_session")
        source.write_text(fortran, encoding="utf-8")
        helper_paths = helper_paths_for_fortran(fortran)
        include_dirs: list[Path] = []
        helper_compile_elapsed = 0.0
        if helper_paths and mode.startswith("gfortran"):
            try:
                r_obj, r_mod_dir, _diag, helper_compile_elapsed = cached_r_helper_for_gfortran(
                    mode,
                    run_dir,
                    timeout,
                )
            except FileNotFoundError:
                elapsed = time.perf_counter() - start
                return RunResult(False, "", "gfortran was not found on PATH", elapsed, ["gfortran"], elapsed, None)
            except subprocess.TimeoutExpired as exc:
                elapsed = time.perf_counter() - start
                return RunResult(False, exc.stdout or "", f"r.f90 compile timed out after {exc.timeout} seconds", elapsed, ["gfortran"], elapsed, None)
            except RuntimeError as exc:
                elapsed = time.perf_counter() - start
                return RunResult(False, "", str(exc), elapsed, ["gfortran"], elapsed, None)
            sources = [r_obj, source]
            include_dirs = [r_mod_dir]
        else:
            sources = [*helper_paths, source]
        compile_cmd = compiler_command(mode, sources, exe, include_dirs=include_dirs)
        compile_start = time.perf_counter()
        try:
            cp = subprocess.run(compile_cmd, cwd=run_dir, text=True, capture_output=True, timeout=timeout)
        except FileNotFoundError:
            elapsed = time.perf_counter() - start
            return RunResult(False, "", f"{compile_cmd[0]} was not found on PATH", elapsed, compile_cmd, elapsed, None)
        except subprocess.TimeoutExpired as exc:
            elapsed = time.perf_counter() - start
            return RunResult(False, exc.stdout or "", f"{mode} compile timed out after {exc.timeout} seconds", elapsed, compile_cmd, elapsed, None)
        compile_elapsed = helper_compile_elapsed + (time.perf_counter() - compile_start)
        if cp.returncode != 0:
            elapsed = time.perf_counter() - start
            return RunResult(False, cp.stdout or "", cp.stderr or "", elapsed, compile_cmd, compile_elapsed, None)
        run_cmd = [str(exe)]
        run_start = time.perf_counter()
        try:
            rp = subprocess.run(run_cmd, cwd=run_dir, text=True, capture_output=True, timeout=timeout)
        except subprocess.TimeoutExpired as exc:
            elapsed = time.perf_counter() - start
            return RunResult(False, exc.stdout or "", f"{mode} run timed out after {exc.timeout} seconds", elapsed, run_cmd, compile_elapsed, None)
        run_elapsed = time.perf_counter() - run_start
        elapsed = time.perf_counter() - start
        diagnostics = "\n".join(part.rstrip() for part in (cp.stdout, cp.stderr, rp.stderr) if part and part.strip())
        return RunResult(rp.returncode == 0, rp.stdout or "", diagnostics, elapsed, run_cmd, compile_elapsed, run_elapsed)


def make_scrolled_text(parent: tk.Widget, *, wrap: str = tk.NONE, height: int = 10) -> tk.Text:
    frame = ttk.Frame(parent)
    frame.grid_rowconfigure(0, weight=1)
    frame.grid_columnconfigure(0, weight=1)
    text = tk.Text(frame, wrap=wrap, undo=True, maxundo=200, height=height)
    yscroll = ttk.Scrollbar(frame, orient=tk.VERTICAL, command=text.yview)
    xscroll = ttk.Scrollbar(frame, orient=tk.HORIZONTAL, command=text.xview)
    text.configure(yscrollcommand=yscroll.set, xscrollcommand=xscroll.set)
    text.grid(row=0, column=0, sticky="nsew")
    yscroll.grid(row=0, column=1, sticky="ns")
    xscroll.grid(row=1, column=0, sticky="ew")
    text.container = frame  # type: ignore[attr-defined]
    return text


def apply_syntax(widget: tk.Text) -> None:
    content = widget.get("1.0", "end-1c")
    for tag in ("keyword", "builtin", "string", "number", "comment"):
        widget.tag_remove(tag, "1.0", tk.END)
    is_fortran = bool(getattr(widget, "is_fortran", False))
    if is_fortran:
        for m in FORTRAN_KEYWORD_RE.finditer(content):
            widget.tag_add("keyword", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        comment_re = FORTRAN_COMMENT_RE
    else:
        for m in R_KEYWORD_RE.finditer(content):
            widget.tag_add("keyword", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        for m in R_BUILTIN_RE.finditer(content):
            widget.tag_add("builtin", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
        comment_re = R_COMMENT_RE
    for m in NUMBER_RE.finditer(content):
        widget.tag_add("number", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
    for m in STRING_RE.finditer(content):
        widget.tag_add("string", f"1.0+{m.start()}c", f"1.0+{m.end()}c")
    for m in comment_re.finditer(content):
        widget.tag_add("comment", f"1.0+{m.start()}c", f"1.0+{m.end()}c")


def configure_text_tags(widget: tk.Text) -> None:
    widget.tag_configure("keyword", foreground="#6b3fa0")
    widget.tag_configure("builtin", foreground="#1c6a8c")
    widget.tag_configure("string", foreground="#9b4a00")
    widget.tag_configure("number", foreground="#285f2a")
    widget.tag_configure("comment", foreground="#6c6c6c")
    widget.tag_configure("source_map_highlight", background="#fff2a8")


class Xr2fIde:
    def __init__(self, root: tk.Tk, *, xr2f: Path, rscript: str, compiler: str, source: Path | None):
        self.root = root
        self.xr2f = xr2f
        self.rscript = rscript
        self.source_path = source
        self.current_fortran = ""
        self.source_to_fortran_lines: dict[int, set[int]] = {}
        self.update_job: str | None = None
        self.highlight_job: str | None = None
        self.raw_r_output = ""
        self.raw_fortran_output = ""

        self.compiler_var = tk.StringVar(value=compiler)
        self.timeout_var = tk.StringVar(value="30")
        self.output_decimals = tk.StringVar(value="")
        self.status_var = tk.StringVar(value="Ready")
        self.elapsed_r_var = tk.StringVar(value="")
        self.elapsed_f_compile_var = tk.StringVar(value="")
        self.elapsed_f_run_var = tk.StringVar(value="")
        self.pretty_var = tk.BooleanVar(value=True)
        self.show_r_output = tk.BooleanVar(value=True)

        self.build_ui()
        if source is not None:
            self.load_source(source)
        self.root.title("xr2f IDE" if source is None else f"xr2f IDE - {source}")

    def build_ui(self) -> None:
        self.root.geometry("1320x840")
        self.root.minsize(900, 600)
        self.root.grid_rowconfigure(1, weight=1)
        self.root.grid_columnconfigure(0, weight=1)

        toolbar = ttk.Frame(self.root, padding=(6, 4))
        toolbar.grid(row=0, column=0, sticky="ew")

        ttk.Button(toolbar, text="Open", command=self.open_source).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(toolbar, text="Save R", command=self.save_source).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(toolbar, text="Save Fortran", command=self.save_fortran).pack(side=tk.LEFT, padx=(0, 10))
        ttk.Button(toolbar, text="Translate", command=self.update_fortran).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(toolbar, text="Run R", command=self.run_r_current).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(toolbar, text="Run Fortran", command=self.run_fortran_current).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(toolbar, text="Run Both", command=self.run_both).pack(side=tk.LEFT, padx=(0, 10))
        ttk.Button(toolbar, text="Clear Output", command=self.clear_output).pack(side=tk.LEFT, padx=(0, 10))

        ttk.Label(toolbar, text="Compiler").pack(side=tk.LEFT)
        ttk.Combobox(toolbar, textvariable=self.compiler_var, values=COMPILER_MODES, width=14, state="readonly").pack(side=tk.LEFT, padx=(4, 10))
        ttk.Checkbutton(toolbar, text="Pretty", variable=self.pretty_var, command=self.update_fortran).pack(side=tk.LEFT, padx=(0, 10))
        ttk.Label(toolbar, text="Timeout").pack(side=tk.LEFT)
        ttk.Entry(toolbar, textvariable=self.timeout_var, width=5).pack(side=tk.LEFT, padx=(4, 10))
        ttk.Label(toolbar, text="Round output").pack(side=tk.LEFT)
        ttk.Entry(toolbar, textvariable=self.output_decimals, width=4).pack(side=tk.LEFT, padx=(4, 10))
        ttk.Button(toolbar, text="Refresh", command=self.refresh_output_format).pack(side=tk.LEFT, padx=(0, 10))
        ttk.Checkbutton(toolbar, text="R output", variable=self.show_r_output, command=self.update_output_layout).pack(side=tk.LEFT)
        ttk.Button(toolbar, text="Help", command=self.show_help).pack(side=tk.RIGHT)

        main_frame = ttk.Frame(self.root)
        main_frame.grid(row=1, column=0, sticky="nsew")
        main_frame.grid_rowconfigure(0, weight=72, uniform="code_output")
        main_frame.grid_rowconfigure(1, weight=28, uniform="code_output")
        main_frame.grid_columnconfigure(0, weight=1)

        editor_pane = ttk.PanedWindow(main_frame, orient=tk.HORIZONTAL)
        editor_pane.grid(row=0, column=0, sticky="nsew")

        r_frame = ttk.LabelFrame(editor_pane, text="R source")
        r_frame.grid_rowconfigure(0, weight=1)
        r_frame.grid_columnconfigure(0, weight=1)
        self.r_text = make_scrolled_text(r_frame, height=28)
        self.r_text.container.grid(row=0, column=0, sticky="nsew")  # type: ignore[attr-defined]
        configure_text_tags(self.r_text)
        self.r_text.bind("<<Modified>>", self.on_source_modified)
        self.r_text.bind("<Return>", self.on_r_return)
        self.r_text.bind("<<Selection>>", self.source_selection_changed)
        editor_pane.add(r_frame, weight=1)

        f_frame = ttk.LabelFrame(editor_pane, text="Generated Fortran")
        f_frame.grid_rowconfigure(0, weight=1)
        f_frame.grid_columnconfigure(0, weight=1)
        self.fortran_text = make_scrolled_text(f_frame, height=28)
        self.fortran_text.is_fortran = True  # type: ignore[attr-defined]
        self.fortran_text.container.grid(row=0, column=0, sticky="nsew")  # type: ignore[attr-defined]
        self.fortran_text.configure(state=tk.DISABLED)
        configure_text_tags(self.fortran_text)
        editor_pane.add(f_frame, weight=1)

        self.output_frame = ttk.Frame(main_frame)
        self.output_frame.grid(row=1, column=0, sticky="nsew")
        self.output_frame.grid_rowconfigure(0, weight=1)
        self.output_frame.grid_columnconfigure(0, weight=1)
        self.output_frame.grid_columnconfigure(1, weight=1)

        self.r_output_frame = ttk.LabelFrame(self.output_frame, text="R output")
        self.r_output_frame.grid_rowconfigure(0, weight=1)
        self.r_output_frame.grid_columnconfigure(0, weight=1)
        self.r_output = make_scrolled_text(self.r_output_frame, wrap=tk.WORD, height=7)
        self.r_output.container.grid(row=0, column=0, sticky="nsew")  # type: ignore[attr-defined]

        self.fortran_output_frame = ttk.LabelFrame(self.output_frame, text="Fortran output")
        self.fortran_output_frame.grid_rowconfigure(0, weight=1)
        self.fortran_output_frame.grid_columnconfigure(0, weight=1)
        self.fortran_output = make_scrolled_text(self.fortran_output_frame, wrap=tk.WORD, height=7)
        self.fortran_output.container.grid(row=0, column=0, sticky="nsew")  # type: ignore[attr-defined]
        self.update_output_layout()

        status = ttk.Frame(self.root, padding=(6, 2))
        status.grid(row=2, column=0, sticky="ew")
        ttk.Label(status, textvariable=self.status_var).pack(side=tk.LEFT)
        ttk.Label(status, textvariable=self.elapsed_f_run_var).pack(side=tk.RIGHT, padx=(4, 0))
        ttk.Label(status, text="Fortran run").pack(side=tk.RIGHT, padx=(8, 0))
        ttk.Label(status, textvariable=self.elapsed_f_compile_var).pack(side=tk.RIGHT, padx=(4, 0))
        ttk.Label(status, text="Fortran compile").pack(side=tk.RIGHT, padx=(8, 0))
        ttk.Label(status, textvariable=self.elapsed_r_var).pack(side=tk.RIGHT, padx=(4, 0))
        ttk.Label(status, text="R").pack(side=tk.RIGHT, padx=(8, 0))

    def timeout_seconds(self) -> float | None:
        raw = self.timeout_var.get().strip()
        try:
            value = float(raw)
        except ValueError:
            self.status_var.set("Invalid timeout; using 30s")
            return 30.0
        if value < 0:
            self.status_var.set("Invalid timeout; using 30s")
            return 30.0
        return None if value == 0 else value

    def source_text(self) -> str:
        return self.r_text.get("1.0", "end-1c")

    def source_name(self) -> str:
        return self.source_path.name if self.source_path is not None else "xr2f_ide_session.R"

    def run_dir(self) -> Path:
        return self.source_path.parent if self.source_path is not None else ROOT

    def on_source_modified(self, _event: tk.Event) -> None:
        if not self.r_text.edit_modified():
            return
        self.r_text.edit_modified(False)
        apply_syntax(self.r_text)
        if self.update_job is not None:
            self.root.after_cancel(self.update_job)
        self.update_job = self.root.after(450, lambda: self.update_fortran(auto=True))

    def on_r_return(self, _event: tk.Event) -> None:
        line = self.r_text.get("insert linestart", "insert lineend")
        if strip_r_comment(line).rstrip().endswith("{") and not r_text_has_open_string(line):
            indent = leading_whitespace(line) + "  "
            self.r_text.insert("insert", "\n" + indent)
            return "break"
        return None

    def update_fortran(self, *, auto: bool = False) -> None:
        source = self.source_text()
        if not source.strip():
            self.current_fortran = ""
            self.source_to_fortran_lines = {}
            self.set_fortran_text("")
            self.status_var.set("Ready")
            return
        if r_source_waiting_for_completion(source, require_enter=auto):
            self.status_var.set("Waiting for complete R statement")
            return
        result = translate_r_to_fortran(
            source,
            xr2f=self.xr2f,
            source_name=self.source_name(),
            pretty=self.pretty_var.get(),
            timeout=self.timeout_seconds(),
        )
        if result.ok:
            self.current_fortran = result.fortran
            shown_fortran = display_fortran(result.fortran)
            self.source_to_fortran_lines = build_source_fortran_map(source.splitlines(), shown_fortran)
            self.set_fortran_text(result.fortran)
            self.set_output(self.fortran_output, "")
            self.status_var.set(f"Translated in {result.elapsed:.3f}s")
        else:
            self.current_fortran = ""
            self.source_to_fortran_lines = {}
            self.set_fortran_text("")
            self.set_output(self.fortran_output, self.format_translate_error(result))
            self.status_var.set(f"Translation failed in {result.elapsed:.3f}s")

    def run_r_current(self) -> None:
        self.show_both_output_panes()
        result = run_r_source(
            self.source_text(),
            rscript=self.rscript,
            source_name=self.source_name(),
            run_dir=self.run_dir(),
            timeout=self.timeout_seconds(),
        )
        self.elapsed_r_var.set(f"{result.elapsed:.3f}s")
        self.set_output(self.r_output, self.format_run_output(result))

    def run_fortran_current(self) -> None:
        if not self.ensure_fortran():
            return
        result = run_fortran_source(
            self.current_fortran,
            mode=self.compiler_var.get(),
            run_dir=self.run_dir(),
            timeout=self.timeout_seconds(),
        )
        self.elapsed_f_compile_var.set(
            f"{result.compile_elapsed:.3f}s" if result.compile_elapsed is not None else ""
        )
        self.elapsed_f_run_var.set(
            f"{result.run_elapsed:.3f}s" if result.run_elapsed is not None else ""
        )
        self.set_output(self.fortran_output, self.format_run_output(result))

    def run_both(self) -> None:
        self.show_both_output_panes()
        self.run_r_current()
        self.run_fortran_current()
        self.show_both_output_panes()

    def show_both_output_panes(self) -> None:
        self.show_r_output.set(True)
        self.update_output_layout()
        self.root.update_idletasks()

    def ensure_fortran(self) -> bool:
        if self.current_fortran.strip():
            return True
        self.update_fortran()
        return bool(self.current_fortran.strip())

    def source_selection_changed(self, _event: tk.Event) -> None:
        self.root.after_idle(self.update_fortran_selection_highlight)

    def update_fortran_selection_highlight(self) -> None:
        previous = str(self.fortran_text.cget("state"))
        self.fortran_text.configure(state=tk.NORMAL)
        self.fortran_text.tag_remove("source_map_highlight", "1.0", tk.END)
        for source_line in selected_text_lines(self.r_text):
            for fortran_line in sorted(self.source_to_fortran_lines.get(source_line, ())):
                self.fortran_text.tag_add("source_map_highlight", f"{fortran_line}.0", f"{fortran_line}.end")
        self.fortran_text.tag_raise("source_map_highlight")
        self.fortran_text.configure(state=previous)

    def format_translate_error(self, result: TranslateResult) -> str:
        parts = [f"$ {' '.join(map(str, result.command))}"]
        if result.stdout.strip():
            parts.append(result.stdout.rstrip())
        if result.stderr.strip():
            parts.append(result.stderr.rstrip())
        return "\n".join(parts)

    def format_run_output(self, result: RunResult) -> str:
        if result.ok and not result.stderr.strip():
            return result.stdout.rstrip()
        parts: list[str] = []
        if not result.ok:
            parts.append(f"$ {' '.join(map(str, result.command))}")
        if result.stderr.strip():
            parts.append(result.stderr.rstrip())
        if result.stdout.strip():
            parts.append(result.stdout.rstrip())
        return "\n".join(parts)

    def format_output_text(self, text: str) -> str:
        raw = self.output_decimals.get().strip()
        if not raw:
            return text
        try:
            decimals = int(raw)
        except ValueError:
            return text
        if decimals < 0:
            return text
        return format_float_tokens(text, decimals)

    def refresh_output_format(self) -> None:
        self.write_output_display(self.r_output, self.raw_r_output)
        self.write_output_display(self.fortran_output, self.raw_fortran_output)

    def set_text(self, widget: tk.Text, text: str) -> None:
        widget.delete("1.0", tk.END)
        if text:
            widget.insert("1.0", text)

    def set_fortran_text(self, text: str) -> None:
        self.fortran_text.configure(state=tk.NORMAL)
        self.set_text(self.fortran_text, display_fortran(text))
        apply_syntax(self.fortran_text)
        self.update_fortran_selection_highlight()
        self.fortran_text.configure(state=tk.DISABLED)

    def set_output(self, widget: tk.Text, text: str) -> None:
        if widget is self.r_output:
            self.raw_r_output = text
        elif widget is self.fortran_output:
            self.raw_fortran_output = text
        self.write_output_display(widget, text)

    def write_output_display(self, widget: tk.Text, text: str) -> None:
        widget.delete("1.0", tk.END)
        text = self.format_output_text(text)
        if text:
            widget.insert("1.0", text + ("\n" if not text.endswith("\n") else ""))

    def clear_output(self) -> None:
        self.set_output(self.r_output, "")
        self.set_output(self.fortran_output, "")
        self.elapsed_r_var.set("")
        self.elapsed_f_compile_var.set("")
        self.elapsed_f_run_var.set("")

    def update_output_layout(self) -> None:
        if self.show_r_output.get():
            self.output_frame.grid_columnconfigure(0, weight=1)
            self.output_frame.grid_columnconfigure(1, weight=1)
            self.r_output_frame.grid(row=0, column=0, sticky="nsew", padx=(0, 3))
            self.fortran_output_frame.grid(row=0, column=1, sticky="nsew", padx=(3, 0))
        else:
            self.r_output_frame.grid_remove()
            self.output_frame.grid_columnconfigure(0, weight=0)
            self.output_frame.grid_columnconfigure(1, weight=1)
            self.fortran_output_frame.grid(row=0, column=1, sticky="nsew", padx=0)

    def open_source(self) -> None:
        path = filedialog.askopenfilename(title="Open R source", filetypes=R_FILETYPES)
        if path:
            self.load_source(Path(path))

    def load_source(self, path: Path) -> None:
        self.source_path = path
        self.set_text(self.r_text, read_text_normalized(path))
        self.r_text.edit_modified(False)
        apply_syntax(self.r_text)
        self.root.title(f"xr2f IDE - {path}")
        self.update_fortran()

    def save_source(self) -> None:
        path = self.source_path
        if path is None:
            selected = filedialog.asksaveasfilename(title="Save R source", filetypes=R_FILETYPES, defaultextension=".R")
            if not selected:
                return
            path = Path(selected)
            self.source_path = path
        path.write_text(self.source_text(), encoding="utf-8")
        self.status_var.set(f"Saved {path}")

    def save_fortran(self) -> None:
        if not self.current_fortran.strip():
            self.update_fortran()
        if not self.current_fortran.strip():
            return
        initial = self.source_path.with_suffix(".f90").name if self.source_path is not None else "xr2f_ide_session.f90"
        selected = filedialog.asksaveasfilename(
            title="Save Fortran source",
            filetypes=FORTRAN_FILETYPES,
            defaultextension=".f90",
            initialfile=initial,
        )
        if not selected:
            return
        Path(selected).write_text(self.current_fortran, encoding="utf-8")
        self.status_var.set(f"Saved {selected}")

    def show_help(self) -> None:
        messagebox.showinfo(
            "xr2f IDE Help",
            "Write R code in the left pane. xr2f.py generates generic Fortran in the right pane.\n\n"
            "Run R executes the original R code with Rscript. Run Fortran compiles and runs the "
            "generated Fortran with the selected standard compiler. If the generated code uses r_mod, "
            "this IDE automatically includes this project's r.f90 helper.\n\n"
            "This IDE intentionally omits ofort-specific REPL, profiling, and compiler features.",
        )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Open an IDE for xr2f.py R-to-generic-Fortran translation.")
    parser.add_argument("source_file", nargs="?", help="optional R source file to open")
    parser.add_argument("--source", help="R source file to open; overrides positional source")
    parser.add_argument("--xr2f", default=str(DEFAULT_XR2F), help="path to xr2f.py")
    parser.add_argument("--rscript", default="Rscript", help="Rscript command")
    parser.add_argument("--compiler", choices=COMPILER_MODES, default="gfortran -O3", help="initial Fortran compiler")
    args = parser.parse_args(argv)

    source_arg = args.source or args.source_file
    source = Path(source_arg) if source_arg else None
    if source is not None and not source.exists():
        print(f"xr2f IDE: source file not found: {source}", file=sys.stderr)
        return 2
    xr2f = Path(args.xr2f)
    if not xr2f.exists():
        print(f"xr2f IDE: xr2f.py not found: {xr2f}", file=sys.stderr)
        return 2

    root = tk.Tk()
    Xr2fIde(root, xr2f=xr2f, rscript=args.rscript, compiler=args.compiler, source=source)
    root.mainloop()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
