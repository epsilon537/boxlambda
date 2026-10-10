#!/usr/bin/env python3
"""Render 12-bit RGB colors from a Forth palette source into a 16x16 PNG grid.

Expected color literals look like: $9ab h,  (also accepts $9ab)
Comments beginning with backslash are ignored for color parsing.
"""
import argparse
import re
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


COLOR_RE = re.compile(r"\$([0-9a-fA-F]{3})\b")


def load_colors(path):
    text = Path(path).read_text(encoding="utf-8")
    # In this Forth source format, backslash starts a comment through EOL.
    lines = [line.split("\\", 1)[0] for line in text.splitlines()]
    colors = []
    for line in lines:
        colors.extend(match.lower() for match in COLOR_RE.findall(line))
    if not colors:
        raise ValueError(f"No 3-digit 12-bit RGB colors found in {path}")
    if len(colors) > 256:
        raise ValueError(
            f"Found {len(colors)} colors; a 16x16 grid holds at most 256."
        )
    return colors


def color_rgb(hex12):
    """Convert RGB444 hex (e.g. '9ab') to 8-bit RGB for PNG rendering."""
    return tuple(int(ch, 16) * 17 for ch in hex12)


def get_font(size):
    for name in ("DejaVuSansMono.ttf", "DejaVuSans.ttf", "LiberationMono-Regular.ttf"):
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            pass
    return ImageFont.load_default()


def render_grid(colors, output, cell_size=96, columns=16, title=None):
    rows = (len(colors) + columns - 1) // columns
    title_height = 42 if title else 0
    image = Image.new("RGB", (columns * cell_size, rows * cell_size + title_height),
                      (245, 245, 245))
    draw = ImageDraw.Draw(image)

    if title:
        title_font = get_font(20)
        draw.text((12, 10), title, fill=(20, 20, 20), font=title_font)

    font = get_font(max(12, cell_size // 6))
    for index, hex12 in enumerate(colors):
        row, col = divmod(index, columns)
        x = col * cell_size
        y = title_height + row * cell_size
        rgb = color_rgb(hex12)
        draw.rectangle((x, y, x + cell_size - 1, y + cell_size - 1),
                       fill=rgb, outline=(90, 90, 90), width=1)

        # Choose readable foreground text using perceived brightness.
        brightness = 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]
        foreground = (0, 0, 0) if brightness > 145 else (255, 255, 255)
        label = hex12
        bbox = draw.textbbox((0, 0), label, font=font)
        text_w = bbox[2] - bbox[0]
        text_h = bbox[3] - bbox[1]
        tx = x + (cell_size - text_w) // 2
        ty = y + (cell_size - text_h) // 2 - bbox[1]
        draw.text((tx, ty), label, fill=foreground, font=font)

    image.save(output)
    print(f"Wrote {output} ({columns}x{rows} cells, {len(colors)} colors)")


def main():
    parser = argparse.ArgumentParser(
        description="Render a 12-bit RGB palette as a labeled PNG grid."
    )
    parser.add_argument("input", help="Forth palette source file")
    parser.add_argument("-o", "--output", help="output PNG (default: INPUT.png)")
    parser.add_argument("--cell-size", type=int, default=96,
                        help="cell width/height in pixels (default: 96)")
    parser.add_argument("--title", help="optional title above the grid")
    args = parser.parse_args()

    if args.cell_size < 24:
        parser.error("--cell-size must be at least 24")

    source = Path(args.input)
    output = Path(args.output) if args.output else source.with_suffix(".png")
    colors = load_colors(source)
    render_grid(colors, output, cell_size=args.cell_size, title=args.title)


if __name__ == "__main__":
    main()
