#!/usr/bin/env python3
import sys
import os
import re
import argparse
import yaml


def evaluate_expressions(entries):
    """
    Evaluates cross-referencing math expressions iteratively by substituting
    resolved constants until every definition drops down to a pure integer.
    """
    resolved = {}
    unresolved = {}

    # Track items that YAML loaded as native integers vs strings
    for e in entries:
        name = e["name"]
        val = e["value"]
        if isinstance(val, int):
            resolved[name] = val
        else:
            unresolved[name] = str(val).strip()

    # Avoid infinite loops on broken mappings
    max_iterations = len(entries) * 2
    iterations = 0

    while unresolved and iterations < max_iterations:
        iterations += 1
        ready_to_resolve = []

        for name, expr_str in list(unresolved.items()):
            # Grab alphanumeric words inside the expression string
            tokens = re.findall(r"\b[A-Za-z_][A-Za-z0-9_]*\b", expr_str)
            # Skip hex prefix keywords
            tokens = [t for t in tokens if t.lower() != "0x"]

            can_resolve = True
            working_expr = expr_str

            for token in tokens:
                if token in resolved:
                    working_expr = re.sub(r"\b" + token + r"\b", str(resolved[token]), working_expr)
                else:
                    can_resolve = False
                    break

            if can_resolve:
                try:
                    # Evaluate the clean mathematical expression string to an integer
                    evaluated_num = eval(working_expr, {"__builtins__": None}, {})
                    ready_to_resolve.append((name, evaluated_num))
                except Exception as err:
                    print(
                        f"Error evaluating math for {name} ('{working_expr}'): {err}",
                        file=sys.stderr,
                    )
                    sys.exit(1)

        for name, num in ready_to_resolve:
            resolved[name] = num
            if name in unresolved:
                del unresolved[name]

        if not ready_to_resolve and unresolved:
            break

    if unresolved:
        print(
            f"Circular or unresolved dependencies detected: {list(unresolved.keys())}",
            file=sys.stderr,
        )
        sys.exit(1)

    return resolved


def format_c(name, val):
    """Formats address keys to 8-digit uppercase hex, and sizes to decimals."""
    if "BASE" in name or "ADDR" in name:
        return f"({name.replace(name, f'0x{val:08X}U')})"
    return str(val)


def format_forth(name, val):
    """Formats address keys to 8-digit lowercase hex, and sizes to decimals."""
    if "BASE" in name or "ADDR" in name:
        return f"${val:08x}"
    return f"#{val}"


def generate_c_header(entries, resolved):
    lines = [
        "#ifndef MEMMAP_H",
        "#define MEMMAP_H",
        "",
        "/* Automatically generated from master definition. Do not edit. */",
        "",
    ]
    for item in entries:
        name = item["name"]
        val = resolved[name]
        c_val = format_c(name, val)
        comment = f" // {item['comment']}" if "comment" in item else ""
        lines.append(f"#define {name:<32} {c_val}{comment}")

    lines.append("")
    lines.append("#endif /* MEMMAP_H */")
    return "\n".join(lines)


def generate_forth_file(entries, resolved):
    lines = ["\\ Automatically generated from master definition. Do not edit.", ""]
    for item in entries:
        name = item["name"]
        val = resolved[name]
        fs_val = format_forth(name, val)
        comment = f" \\ {item['comment']}" if "comment" in item else ""
        lines.append(f"{fs_val:<16} constant {name}{comment}")

    lines.append("")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Generate fully evaluated C headers and Forth maps from a master YAML layout."
    )
    parser.add_argument(
        "-i",
        "--input",
        default="memmap.yaml",
        help="Path to input master YAML file (default: memmap.yaml)",
    )
    parser.add_argument(
        "-c",
        "--c-header",
        default="memmap.h",
        help="Output path for C header file (default: memmap.h)",
    )
    parser.add_argument(
        "-f",
        "--forth",
        default="memmap.fs",
        help="Output path for Forth map file (default: memmap.fs)",
    )

    args = parser.parse_args()

    if not os.path.exists(args.input):
        print(f"Error: Could not find input file: '{args.input}'", file=sys.stderr)
        sys.exit(1)

    with open(args.input, "r") as f:
        try:
            config = yaml.safe_load(f)
        except Exception as e:
            print(f"Failed parsing YAML layout structure: {e}", file=sys.stderr)
            sys.exit(1)

    entries = config.get("memory_map", [])
    resolved_values = evaluate_expressions(entries)

    # Write files out cleanly
    with open(args.c_header, "w") as f:
        f.write(generate_c_header(entries, resolved_values))
    print(f"Generated {args.c_header} successfully.")

    with open(args.forth, "w") as f:
        f.write(generate_forth_file(entries, resolved_values))
    print(f"Generated {args.forth} successfully.")


if __name__ == "__main__":
    main()
