import os
from pathlib import Path

# Configuration
DOCS_DIR = Path("docs")
GITHUB_PREFIX = "https://github.com/epsilon537/boxlambda/blob/master"

def compute_relative_root(md_file: Path) -> str:
    """
    Compute relative path from md_file to repo root.
    docs/ is one level below root.
    """
    rel_path = md_file.relative_to(DOCS_DIR)
    depth = len(rel_path.parents) - 1  # exclude the file itself

    # +1 to go from docs/ to repo root
    total_up = depth + 1

    if total_up == 0:
        return "."
    return "/".join([".."] * total_up)


def process_file(md_file: Path):
    rel_root = compute_relative_root(md_file)

    with open(md_file, "r", encoding="utf-8") as f:
        content = f.read()

    if GITHUB_PREFIX not in content:
        return  # nothing to do

    # Replace prefix with computed relative root
    new_content = content.replace(GITHUB_PREFIX, rel_root)

    with open(md_file, "w", encoding="utf-8") as f:
        f.write(new_content)

    print(f"Updated: {md_file} → {rel_root}")


def main():
    for md_file in DOCS_DIR.rglob("*.md"):
        process_file(md_file)


if __name__ == "__main__":
    main()
