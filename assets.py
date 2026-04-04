import re
from pathlib import Path

DOCS_DIR = Path("docs")

def compute_assets_prefix(md_file: Path) -> str:
    """
    Compute relative path from md_file to docs/assets/
    """
    rel_path = md_file.relative_to(DOCS_DIR)
    depth = len(rel_path.parents) - 1  # exclude file itself

    if depth == 0:
        return "assets"
    return "/".join([".."] * depth) + "/assets"


def process_file(md_file: Path):
    prefix = compute_assets_prefix(md_file)

    with open(md_file, "r", encoding="utf-8") as f:
        content = f.read()

    # Match assets/... but NOT already prefixed (../assets, ./assets, http...)
    pattern = r'(?<![./])\bassets/([^\s)"]+)'

    def replacer(match):
        path = match.group(1)
        return f"{prefix}/{path}"

    new_content = re.sub(pattern, replacer, content)

    if new_content != content:
        with open(md_file, "w", encoding="utf-8") as f:
            f.write(new_content)
        print(f"Updated: {md_file} → {prefix}/assets")


def main():
    for md_file in DOCS_DIR.rglob("*.md"):
        process_file(md_file)


if __name__ == "__main__":
    main()

