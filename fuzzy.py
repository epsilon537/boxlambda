import re
import os
from pathlib import Path

DOCS_DIR = Path("docs")

MD_LINK_PATTERN = re.compile(r'\[([^\]]+)\]\(([^)]+\.md)\)')


def tokenize(name: str):
    """Split filename into meaningful tokens"""
    name = name.lower().replace(".md", "")
    return re.split(r'[_\-/]+', name)


def score_match(tokens, candidate_path: Path):
    """
    Score how well a candidate matches tokens.
    Higher = better.
    """
    candidate_str = str(candidate_path).lower()
    score = 0

    for t in tokens:
        if t in candidate_str:
            score += 1

    return score


def find_best_match(link: str, all_files):
    tokens = tokenize(link)

    best_score = 0
    best_match = None
    ties = []

    for f in all_files:
        s = score_match(tokens, f)
        if s > best_score:
            best_score = s
            best_match = f
            ties = [f]
        elif s == best_score and s > 0:
            ties.append(f)

    if best_score == 0:
        return None

    if len(ties) > 1:
        print(f"AMBIGUOUS: {link} → {[str(t) for t in ties]}")

    return best_match


def compute_relative_path(from_path: Path, to_path: Path) -> str:
    return os.path.relpath(to_path, start=from_path.parent)


def process_file(md_file: Path, all_files):
    with open(md_file, "r", encoding="utf-8") as f:
        content = f.read()

    def replacer(match):
        text = match.group(1)
        link = match.group(2)

        # Skip external or already structured links
        if link.startswith("http") or "/" in link:
            return match.group(0)

        best = find_best_match(link, all_files)

        if not best:
            print(f"NOT FOUND: {link} (from {md_file})")
            return match.group(0)

        rel_path = compute_relative_path(md_file, best)

        return f"[{text}]({rel_path})"

    new_content = MD_LINK_PATTERN.sub(replacer, content)

    if new_content != content:
        with open(md_file, "w", encoding="utf-8") as f:
            f.write(new_content)
        print(f"Updated: {md_file}")


def main():
    all_files = list(DOCS_DIR.rglob("*.md"))

    for md_file in all_files:
        process_file(md_file, all_files)


if __name__ == "__main__":
    main()

