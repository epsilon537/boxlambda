#!/usr/bin/env python3

import sys


def convert_color(line):
    line = line.strip()

    if not line or line.startswith("#"):
        return line

    rgb = [int(line[i : i + 2], 16) // 17 for i in range(0, 6, 2)]
    return "".join(f"{value:x}" for value in rgb)


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} FILE", file=sys.stderr)
        sys.exit(1)

    with open(sys.argv[1]) as f:
        for line in f:
            print(convert_color(line))


if __name__ == "__main__":
    main()
