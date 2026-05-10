import yaml
import os
from pathlib import Path

MKDOCS_FILE = "mkdocs.yml"
OUTPUT_FILE = "site_docs/full-index.md"
DOCS_DIR = Path("site_docs")

class EnvLoader(yaml.SafeLoader):
    pass

def env_constructor(loader, node):
    value = loader.construct_scalar(node)
    return os.environ.get(value, "")

EnvLoader.add_constructor("!ENV", env_constructor)

def load_nav():
    with open("mkdocs.yml", "r") as f:
        cfg = yaml.load(f, Loader=EnvLoader)
    return cfg.get("nav", [])

def format_link(title, path):
    return f"- [{title}]({path})"

def walk(nav, depth=0):
    lines = []
    indent = "  " * depth

    for item in nav:
        if isinstance(item, dict):
            for title, value in item.items():
                if isinstance(value, str):
                    lines.append(indent + format_link(title, value))
                elif isinstance(value, list):
                    lines.append(f"{indent}- **{title}**")
                    lines.extend(walk(value, depth + 1))
        else:
            # rare case: plain string
            lines.append(indent + f"- {item}")

    return lines


def generate():
    nav = load_nav()

    lines = [
        "# Full Documentation Index",
        "",
        "Complete structured overview of all documentation pages.",
        "",
    ]

    lines.extend(walk(nav))

    output_path = Path(OUTPUT_FILE)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with open(output_path, "w") as f:
        f.write("\n".join(lines))

    print(f"Generated {OUTPUT_FILE}")


if __name__ == "__main__":
    generate()
