#!/usr/bin/env python3

import os
from pathlib import Path

DOCS_DIR = Path("docs")
ROOT_README = Path("README.md")


def collect_renames(base):
    """Collect files and directories to rename."""
    rename_map = {}

    # Include both files and directories
    for path in base.rglob("*"):
        if "_" in path.name:
            new_name = path.name.replace("_", "-")
            new_path = path.with_name(new_name)
            rename_map[path] = new_path

    return rename_map


def sort_paths_for_rename(rename_map):
    """
    Sort paths so deeper paths are renamed first.
    This avoids breaking parent paths before children are processed.
    """
    return sorted(rename_map.items(), key=lambda x: -len(x[0].parts))


def apply_renames(rename_items):
    """Rename files and directories."""
    for old, new in rename_items:
        if not old.exists():
            continue  # may have been renamed already as part of a parent move
        print(f"Renaming: {old} -> {new}")
        old.rename(new)


def build_string_map(rename_items, base):
    """Build mapping of old → new relative paths."""
    str_map = {}

    for old, new in rename_items:
        try:
            old_rel = old.relative_to(base).as_posix()
            new_rel = new.relative_to(base).as_posix()
            str_map[old_rel] = new_rel
        except ValueError:
            pass  # skip anything not under base (shouldn't happen)

        # Also map just the name
        str_map[old.name] = new.name

    return str_map


def update_markdown_file(md_file, str_map):
    content = md_file.read_text(encoding="utf-8")
    updated = content

    for old, new in str_map.items():
        updated = updated.replace(old, new)

    if updated != content:
        print(f"Updating links in: {md_file}")
        md_file.write_text(updated, encoding="utf-8")


def update_all_markdown(base, str_map):
    """Update all markdown files under docs/ and root README.md."""
    for md_file in base.rglob("*.md"):
        update_markdown_file(md_file, str_map)

    if ROOT_README.exists():
        update_markdown_file(ROOT_README, str_map)


def main():
    rename_map = collect_renames(DOCS_DIR)

    if not rename_map:
        print("No files or directories to rename.")
        return

    rename_items = sort_paths_for_rename(rename_map)

    # Build mapping BEFORE renaming (important!)
    str_map = build_string_map(rename_items, DOCS_DIR)

    apply_renames(rename_items)
    update_all_markdown(DOCS_DIR, str_map)

    print("Done.")


if __name__ == "__main__":
    main()
