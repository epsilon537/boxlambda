#!/usr/bin/env python3
"""
export_docs.py

Export docs/ into site_docs/ for Read the Docs / MkDocs.

Features:
- Copies docs/ tree to site_docs/
- Rewrites Markdown links that point to source-code files/directories
  into GitHub blob/tree URLs
- Preserves normal docs-to-docs relative links
- Supports branch/tag selection with --ref

Usage:
    python3 tools/export_docs.py --ref develop
    python3 tools/export_docs.py --ref v1.0.0
"""

from pathlib import Path
import shutil
import re
import argparse

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------

REPO_URL = "https://github.com/epsilon537/boxlambda"

DOCS_SRC = Path("docs")
DOCS_DST = Path("site_docs")

# Top-level repo dirs that should become GitHub links
CODE_ROOTS = {
    "sw",
    "gw",
    "rtl",
    "hdl",
    "sim",
    "scripts",
    "tools",
    "board",
    "fs",
    "registers",
}

# File extensions usually worth linking as source
CODE_EXTS = {
    ".c", ".h", ".cpp", ".hpp",
    ".S", ".s",
    ".ld",
    ".py",
    ".v", ".sv", ".vh",
    ".vhd",
    ".tcl",
    ".mk",
    ".sh",
    ".txt",
    ".fs",
}

# Markdown inline link pattern: [text](target)
LINK_RE = re.compile(r'\[([^\]]+)\]\(([^)]+)\)')


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

def is_external(target: str) -> bool:
    return target.startswith(("http://", "https://", "mailto:", "#"))


def strip_anchor(target: str):
    if "#" in target:
        path, anchor = target.split("#", 1)
        return path, "#" + anchor
    return target, ""


def repo_rel_from_doc(md_file: Path, target: str):
    """
    Resolve relative link target from markdown file to repo-relative path.
    md_file is inside docs/.
    """
    source_dir = md_file.parent
    resolved = (source_dir / target).resolve()

    try:
        return resolved.relative_to(Path.cwd().resolve())
    except Exception:
        return None


def should_rewrite(repo_rel: Path) -> bool:
    if not repo_rel.parts:
        return False

    top = repo_rel.parts[0]

    # If points into docs/, keep internal
    if top == "docs":
        return False

    if top in CODE_ROOTS:
        return True

    # Also rewrite files by extension anywhere outside docs
    if repo_rel.suffix in CODE_EXTS:
        return True

    return False


def github_url(repo_rel: Path, ref: str):
    repo_posix = repo_rel.as_posix()

    if repo_rel.suffix:
        return f"{REPO_URL}/blob/{ref}/{repo_posix}"
    else:
        return f"{REPO_URL}/tree/{ref}/{repo_posix}"


def rewrite_markdown(md_path: Path, ref: str):
    text = md_path.read_text(encoding="utf-8")

    def repl(match):
        label = match.group(1)
        target = match.group(2).strip()

        if is_external(target):
            return match.group(0)

        raw_path, anchor = strip_anchor(target)

        # Ignore empty / malformed
        if not raw_path:
            return match.group(0)

        repo_rel = repo_rel_from_doc(md_path, raw_path)
        if repo_rel is None:
            return match.group(0)

        if should_rewrite(repo_rel):
            new_target = github_url(repo_rel, ref) + anchor
            return f"[{label}]({new_target})"

        # keep docs/internal links untouched
        return match.group(0)

    new_text = LINK_RE.sub(repl, text)

    md_path.write_text(new_text, encoding="utf-8")


# ------------------------------------------------------------
# Main
# ------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ref", default="develop",
                    help="Git ref to use for GitHub links (branch/tag)")
    args = ap.parse_args()

    if not DOCS_SRC.exists():
        raise SystemExit("docs/ not found")

    if DOCS_DST.exists():
        shutil.rmtree(DOCS_DST)

    shutil.copytree(DOCS_SRC, DOCS_DST)

    for md in DOCS_DST.rglob("*.md"):
        rewrite_markdown(md, args.ref)

    print(f"Exported docs -> {DOCS_DST}/ using ref '{args.ref}'")


if __name__ == "__main__":
    main()

