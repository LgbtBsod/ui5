#!/usr/bin/env python3
"""Conservative dependency audit for the whole repository.

Builds a file-reference graph using:
- Python imports (`import x`, `from x import y`)
- JS sap.ui.define dependencies and import/from/require patterns
- Generic textual path references in xml/json/yaml/md/sh

Outputs JSON report with:
- reachable files from configured entrypoints
- unreferenced candidates (not auto-deletable; manual review required)
"""
from __future__ import annotations

import ast
import json
import re
from pathlib import Path
from collections import defaultdict, deque

ROOT = Path(__file__).resolve().parents[1]

EXCLUDE_DIRS = {".git", "node_modules", "__pycache__"}
TEXT_EXTS = {
    ".py", ".js", ".xml", ".json", ".yml", ".yaml", ".md", ".sh", ".properties", ".html", ".less", ".css"
}

ENTRYPOINTS = [
    "index.html",
    "Component.js",
    "manifest.json",
    ".github/workflows/smoke-gate.yml",
    "mock_gate_way/main.py",
    "mock_gate_way/scripts/seed_test_data.py",
    "scripts/pre-push-smoke.sh",
    "scripts/ci/enterprise-ux-gate.sh",
]


def rel(p: Path) -> str:
    return p.relative_to(ROOT).as_posix()


def all_files() -> list[Path]:
    files = []
    for p in ROOT.rglob("*"):
        if not p.is_file():
            continue
        if any(part in EXCLUDE_DIRS for part in p.parts):
            continue
        files.append(p)
    return files


ALL = all_files()
REL_MAP = {rel(p): p for p in ALL}
BY_NAME = defaultdict(list)
for rp in REL_MAP:
    BY_NAME[Path(rp).name].append(rp)


def resolve_path_like(base: Path, token: str) -> str | None:
    token = token.strip().strip('"\'`')
    if not token or token.startswith("http://") or token.startswith("https://"):
        return None

    candidates = []
    if token.startswith("/"):
        token = token.lstrip("/")
        if token in REL_MAP:
            return token
        candidates.append(token)
    else:
        c = (base.parent / token).resolve()
        try:
            rc = c.relative_to(ROOT).as_posix()
            candidates.append(rc)
        except Exception:
            pass
        candidates.append(token)

    exts = ["", ".js", ".py", ".xml", ".json", ".yml", ".yaml", ".md", ".sh"]
    for cand in list(candidates):
        for ext in exts:
            k = cand + ext
            if k in REL_MAP:
                return k

    name = Path(token).name
    if name in BY_NAME and len(BY_NAME[name]) == 1:
        return BY_NAME[name][0]
    return None


def py_deps(p: Path) -> set[str]:
    out: set[str] = set()
    try:
        tree = ast.parse(p.read_text(encoding="utf-8"))
    except Exception:
        return out

    module = rel(p).removesuffix(".py").replace("/", ".")
    pkg_parts = module.split(".")[:-1]

    for n in ast.walk(tree):
        if isinstance(n, ast.Import):
            for a in n.names:
                mod = a.name.replace(".", "/") + ".py"
                if mod in REL_MAP:
                    out.add(mod)
        elif isinstance(n, ast.ImportFrom):
            if n.module is None and n.level <= 0:
                continue
            base_parts = pkg_parts.copy()
            if n.level:
                up = max(0, len(base_parts) - n.level + 1)
                base_parts = base_parts[:up]
            mod_parts = [] if not n.module else n.module.split(".")
            full = base_parts + mod_parts
            mod_file = "/".join(full) + ".py"
            if mod_file in REL_MAP:
                out.add(mod_file)
            for a in n.names:
                sub = "/".join(full + [a.name]) + ".py"
                if sub in REL_MAP:
                    out.add(sub)
    return out


def js_deps(p: Path) -> set[str]:
    text = p.read_text(encoding="utf-8", errors="ignore")
    out: set[str] = set()
    # sap.ui.define(["a/b/C"])
    for m in re.finditer(r"sap\.ui\.define\s*\(\s*\[(.*?)\]", text, re.S):
        arr = m.group(1)
        for s in re.findall(r"['\"]([^'\"]+)['\"]", arr):
            if s.startswith("sap/"):
                continue
            r = resolve_path_like(p, s)
            if r:
                out.add(r)
            elif s.endswith(".js") and s in REL_MAP:
                out.add(s)
            else:
                k = s + ".js"
                if k in REL_MAP:
                    out.add(k)
    # import/require
    for s in re.findall(r"(?:import\s+.*?from\s+|require\()\s*['\"]([^'\"]+)['\"]", text):
        r = resolve_path_like(p, s)
        if r:
            out.add(r)
    return out


def text_refs(p: Path) -> set[str]:
    text = p.read_text(encoding="utf-8", errors="ignore")
    out: set[str] = set()
    for tok in re.findall(r"[A-Za-z0-9_./-]+\.[A-Za-z0-9_]+", text):
        if tok.count("/") == 0 and tok not in REL_MAP:
            continue
        r = resolve_path_like(p, tok)
        if r:
            out.add(r)
    return out


def build_graph() -> dict[str, set[str]]:
    g: dict[str, set[str]] = {rp: set() for rp in REL_MAP}
    for rp, p in REL_MAP.items():
        if p.suffix == ".py":
            g[rp] |= py_deps(p)
        if p.suffix == ".js":
            g[rp] |= js_deps(p)
        if p.suffix in TEXT_EXTS:
            g[rp] |= text_refs(p)
    return g


def reachable(graph: dict[str, set[str]], starts: list[str]) -> set[str]:
    seen: set[str] = set()
    q = deque([s for s in starts if s in graph])
    while q:
        cur = q.popleft()
        if cur in seen:
            continue
        seen.add(cur)
        for nxt in graph.get(cur, ()):
            if nxt not in seen:
                q.append(nxt)
    return seen


def main() -> int:
    graph = build_graph()
    starts = [s for s in ENTRYPOINTS if s in graph]
    seen = reachable(graph, starts)

    candidates = []
    for rp in sorted(graph):
        p = REL_MAP[rp]
        if rp in seen:
            continue
        # skip binary/large assets and generated themes from auto-delete candidates
        if rp.startswith("themes/") or rp.startswith("docs/artifacts/") or p.suffix in {".svg", ".png", ".jpg", ".woff", ".woff2", ".ttf"}:
            continue
        candidates.append(rp)

    report = {
        "entrypoints": starts,
        "reachable_count": len(seen),
        "total_files": len(graph),
        "unreferenced_candidates_count": len(candidates),
        "unreferenced_candidates": candidates,
        "note": "Conservative static scan. Manual review required before deletion.",
    }

    out = ROOT / "docs" / "artifacts" / "dependency-audit-report.json"
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {out.relative_to(ROOT)}; candidates={len(candidates)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
