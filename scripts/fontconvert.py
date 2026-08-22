#!/usr/bin/env python3

import re
import sys

filename = sys.argv[1]

text = open(filename).read()

# Extract all hexadecimal byte values from the bitmap array.
m = re.search(r"FONT_[A-Z0-9_]+_BITMAP\s*\[\]\s*=\s*\{(.*?)\};", text, re.S)

if not m:
    raise SystemExit("Could not find bitmap array")

data = [int(x, 16) for x in re.findall(r"0x([0-9a-fA-F]{2})", m.group(1))]

if len(data) % 8:
    raise SystemExit(f"Font contains {len(data)} bytes, not a multiple of 8")

num_glyphs = len(data) // 8

for index in range(num_glyphs):
    glyph = data[index * 8 : index * 8 + 8]

    label = f"# ${index:02x} ASC: ${index + 32:02x} '{chr(index + 32)}'"

    print(label)
    print()

    for row in glyph:
        print("".join("*" if row & (0x80 >> bit) else "." for bit in range(8)))

    print()
