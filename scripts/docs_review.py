#!/usr/bin/env python3
"""Read-only editorial audit of MkDocs Markdown prose using OpenRouter."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

SYSTEM = """You are an editorial reviewer for BoxLambda, an open-source FPGA-based microcomputer project. Review the supplied Markdown prose for awkward phrasing, wordiness, repetition, ambiguity, inconsistent terminology, unnecessary formality, grammar, punctuation, and spelling. Aim for clear, concise, natural technical English suitable for open-source developer documentation. Preserve technical meaning; do not invent facts or change terminology just to sound conventional. Do not suggest changes to inline code, identifiers, commands, paths, URLs, or link destinations. Code blocks and diagrams have already been removed. Prefer small, targeted improvements; leave good prose alone.

Assign each actionable finding an integer severity from 0 to 5:
0 = minor nitpick; optional stylistic preference.
1 = very minor improvement.
2 = useful but nonessential improvement.
3 = worthwhile change that improves clarity or readability.
4 = important change; the wording is significantly unclear, awkward, or misleading.
5 = must-change; a serious error, ambiguity, or misleading statement.

Be conservative. Do not inflate severity, and do not report subjective preferences as important problems.

Return only actionable findings as valid JSON:
{"findings":[{"original":"exact quote","replacement":"suggested wording","reason":"brief explanation","severity":0}]}
The severity value must be an integer from 0 to 5. If there are no useful improvements, return {"findings":[]}. Do not wrap JSON in Markdown fences."""

SKIP_DIRS = {
    ".git",
    ".venv",
    "venv",
    "site",
    "build",
    "dist",
    "node_modules",
    "__pycache__",
    ".cache",
}


def digest(s):
    return hashlib.sha256(s.encode()).hexdigest()


def extract_blocks(text):
    """Keep prose blocks and their start lines; remove code and diagrams."""
    lines = text.splitlines()
    out = []
    buf = []
    start = 1
    fence = None
    html_skip = False
    front = bool(lines and lines[0].strip() == "---")

    def flush():
        nonlocal buf
        value = "\n".join(buf).strip()
        if value:
            out.append((start, value))
        buf = []

    for n, line in enumerate(lines, 1):
        s = line.strip()

        if front:
            if n > 1 and s in ("---", "..."):
                front = False
            continue

        if html_skip:
            if re.search(r"</\s*(pre|code|svg)\s*>", line, re.I):
                html_skip = False
            continue

        if re.search(r"<\s*(pre|svg|code)\b", line, re.I):
            flush()
            if not re.search(r"</\s*(pre|code|svg)\s*>", line, re.I):
                html_skip = True
            continue

        m = re.match(r"^\s{0,3}(`{3,}|~{3,})", line)

        if fence:
            if re.match(r"^\s{0,3}" + re.escape(fence[0]) + "{" + str(fence[1]) + r",}\s*$", line):
                fence = None
            continue

        if m:
            flush()
            mark = m.group(1)
            fence = (mark[0], len(mark))
            continue

        if line.startswith("    ") or line.startswith("\t"):
            flush()
            continue

        if not s:
            flush()
            continue

        if re.match(r"^\s{0,3}#{1,6}\s+", line):
            flush()
            start = n
            buf = [line]
            flush()
            continue

        if not buf:
            start = n
        buf.append(line)

    flush()
    return out


def make_chunks(path, root, max_chars):
    try:
        blocks = extract_blocks(path.read_text(encoding="utf-8"))
    except UnicodeDecodeError:
        print(f"Skipping non-UTF-8 file: {path}", file=sys.stderr)
        return []

    rel = path.relative_to(root).as_posix()
    chunks = []
    cur = []
    size = 0

    def emit():
        nonlocal cur, size
        if cur:
            chunks.append(
                {
                    "path": rel,
                    "start_line": cur[0][0],
                    "end_line": cur[-1][0] + cur[-1][1].count("\n"),
                    "text": "\n\n".join(b for _, b in cur),
                }
            )
        cur = []
        size = 0

    for line_no, block in blocks:
        pieces = []

        if len(block) <= max_chars:
            pieces = [block]
        else:
            part = []
            plen = 0
            for line in block.splitlines():
                if part and plen + len(line) + 1 > max_chars:
                    pieces.append("\n".join(part))
                    part = []
                    plen = 0
                part.append(line)
                plen += len(line) + 1
            if part:
                pieces.append("\n".join(part))

        for piece in pieces:
            if cur and size + len(piece) + 2 > max_chars:
                emit()
            cur.append((line_no, piece))
            size += len(piece) + 2

    emit()
    return chunks


def request_openrouter(model, api_key, prompt, timeout):
    body = {
        "model": model,
        "temperature": 0.1,
        "max_tokens": 2500,
        "response_format": {"type": "json_object"},
        "messages": [{"role": "system", "content": SYSTEM}, {"role": "user", "content": prompt}],
    }

    req = urllib.request.Request(
        "https://openrouter.ai/api/v1/chat/completions",
        data=json.dumps(body).encode(),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://github.com/epsilon537/boxlambda",
            "X-OpenRouter-Title": "BoxLambda Documentation Audit",
        },
        method="POST",
    )

    try:
        with urllib.request.urlopen(req, timeout=timeout) as response:
            data = json.loads(response.read().decode())
    except urllib.error.HTTPError as e:
        raise RuntimeError(f"OpenRouter HTTP {e.code}: {e.read().decode(errors='replace')}") from e
    except urllib.error.URLError as e:
        raise RuntimeError(f"OpenRouter connection error: {e}") from e

    try:
        result = json.loads(data["choices"][0]["message"]["content"])
    except (KeyError, IndexError, TypeError, json.JSONDecodeError) as e:
        raise RuntimeError(f"Invalid JSON response: {data!r}") from e

    return result, data.get("usage") or {}


def get_min_severity(value):
    """Use the CLI value or interactively ask for a minimum severity."""
    if value is not None:
        return value

    print(
        "\nMinimum severity of suggestions:\n"
        "  0 = minor nitpick (show everything)\n"
        "  1 = very minor improvement\n"
        "  2 = useful but nonessential\n"
        "  3 = worthwhile improvement\n"
        "  4 = important change\n"
        "  5 = must-change only"
    )

    while True:
        try:
            answer = input("Minimum severity [0]: ").strip()
        except EOFError:
            return 0

        if answer == "":
            return 0

        try:
            level = int(answer)
            if 0 <= level <= 5:
                return level
        except ValueError:
            pass

        print("Please enter an integer from 0 to 5.")


def main():
    ap = argparse.ArgumentParser(
        description="Audit MkDocs prose with OpenRouter; never edits source Markdown."
    )
    ap.add_argument(
        "--docs", type=Path, default=Path("docs"), help="Markdown directory (default: docs)"
    )
    ap.add_argument(
        "--model",
        default=os.getenv("OPENROUTER_MODEL", "google/gemini-2.5-flash"),
        help="OpenRouter model ID",
    )
    ap.add_argument("--report", type=Path, default=Path("docs-review.md"))
    ap.add_argument("--cache", type=Path, default=Path(".docs-review-cache.json"))
    ap.add_argument(
        "--max-chars", type=int, default=12000, help="Maximum approximate characters per request"
    )
    ap.add_argument("--limit", type=int, default=0, help="Process only first N chunks (0 = all)")
    ap.add_argument(
        "--min-severity",
        type=int,
        choices=range(6),
        default=None,
        help="Minimum severity to report (0-5); prompts if omitted",
    )
    ap.add_argument("--dry-run", action="store_true", help="Count chunks without API calls")
    ap.add_argument("--timeout", type=int, default=120)
    ap.add_argument("--delay", type=float, default=0.4)
    args = ap.parse_args()

    if args.max_chars < 1000:
        ap.error("--max-chars must be at least 1000")

    min_severity = get_min_severity(args.min_severity)
    print(f"\nMinimum severity selected: {min_severity}\n")

    key = os.getenv("OPENROUTER_API_KEY")
    if not key and not args.dry_run:
        ap.error("Set OPENROUTER_API_KEY in your environment first.")

    docs = args.docs.resolve()
    if not docs.is_dir():
        ap.error(f"Directory not found: {docs}")

    root = docs.parent
    files = sorted(p for p in docs.rglob("*.md") if not any(part in SKIP_DIRS for part in p.parts))

    chunks = []
    for p in files:
        chunks.extend(make_chunks(p, root, args.max_chars))

    if args.limit:
        chunks = chunks[: args.limit]

    print(f"Markdown files: {len(files)}\nProse chunks: {len(chunks)}\nModel: {args.model}")

    if args.dry_run:
        for c in chunks[:30]:
            print(f"  {c['path']}:{c['start_line']} ({len(c['text'])} chars)")
        if len(chunks) > 30:
            print(f"  ... and {len(chunks) - 30} more")
        return 0

    try:
        cache = json.loads(args.cache.read_text()) if args.cache.exists() else {}
    except (OSError, json.JSONDecodeError):
        cache = {}

    findings = []
    errors = []
    reused = 0
    prompt_tokens = 0
    completion_tokens = 0

    for i, c in enumerate(chunks, 1):
        ck = digest(
            json.dumps(
                {"model": args.model, "system": SYSTEM, "path": c["path"], "text": c["text"]},
                sort_keys=True,
                ensure_ascii=False,
            )
        )

        if ck in cache:
            result = cache[ck]["result"]
            usage = cache[ck].get("usage", {})
            reused += 1
            print(f"[{i}/{len(chunks)}] cached {c['path']}:{c['start_line']}")
        else:
            prompt = (
                f"Review this excerpt from `{c['path']}`, "
                f"lines {c['start_line']}-{c['end_line']}. "
                "The excerpt may begin or end mid-page; assess only "
                "what is shown and quote originals exactly.\n\n"
                "--- BEGIN PROSE ---\n"
                f"{c['text']}\n"
                "--- END PROSE ---"
            )

            print(f"[{i}/{len(chunks)}] reviewing {c['path']}:{c['start_line']}")

            try:
                result, usage = request_openrouter(args.model, key, prompt, args.timeout)
            except Exception as e:
                errors.append(f"{c['path']}:{c['start_line']}: {e}")
                print(f"  ERROR: {e}", file=sys.stderr)
                continue

            cache[ck] = {"result": result, "usage": usage}
            args.cache.parent.mkdir(parents=True, exist_ok=True)
            args.cache.write_text(json.dumps(cache, ensure_ascii=False, indent=2), encoding="utf-8")
            time.sleep(max(0, args.delay))

        prompt_tokens += int(usage.get("prompt_tokens") or 0)
        completion_tokens += int(usage.get("completion_tokens") or 0)

        items = result.get("findings", [])
        if isinstance(items, list):
            for f in items:
                if not isinstance(f, dict):
                    continue
                try:
                    severity = int(f.get("severity", 0))
                except (ValueError, TypeError):
                    severity = 0

                if not 0 <= severity <= 5:
                    severity = 0

                if severity < min_severity:
                    continue

                findings.append(
                    {
                        **f,
                        "severity": severity,
                        "path": c["path"],
                        "start_line": c["start_line"],
                        "end_line": c["end_line"],
                    }
                )

    unique = []
    seen = set()

    for f in findings:
        k = (f.get("path", ""), f.get("original", ""), f.get("replacement", ""))
        if k not in seen:
            seen.add(k)
            unique.append(f)

    unique.sort(key=lambda f: (f.get("path", ""), -f.get("severity", 0), f.get("start_line", 0)))

    out = [
        "# BoxLambda Documentation Review",
        "",
        f"- Model: `{args.model}`",
        f"- Markdown files scanned: {len(files)}",
        f"- Chunks: {len(chunks)}",
        f"- Minimum severity: {min_severity}",
        f"- Cached chunks reused: {reused}",
        f"- Findings meeting severity threshold: {len(unique)}",
        "- Source files were not modified.",
        "",
        "> Severity: 0 = nitpick; 1 = very minor; 2 = useful but "
        "nonessential; 3 = worthwhile; 4 = important; 5 = must-change.",
        "> Review suggestions manually before applying them. "
        "Reported line ranges identify the chunk, not necessarily "
        "the exact sentence line.",
        "",
    ]

    current = None
    for f in unique:
        path = f.get("path", "unknown")
        if path != current:
            out += [f"## `{path}`", ""]
            current = path

        out += [
            f"### Severity {f.get('severity', 0)}/5 — "
            f"lines {f.get('start_line', '?')}–{f.get('end_line', '?')}",
            "",
            "**Original**",
            "",
            "> " + str(f.get("original", "")).replace("\n", "\n> "),
            "",
            "**Suggested replacement**",
            "",
            "> " + str(f.get("replacement", "")).replace("\n", "\n> "),
            "",
            f"**Why:** {f.get('reason', '')}",
            "",
        ]

    if not unique:
        out += [f"No findings with severity {min_severity} or higher.", ""]

    if errors:
        out += ["## Errors", ""] + ["- " + e for e in errors] + [""]

    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text("\n".join(out), encoding="utf-8")

    print(
        f"\nReport: {args.report}\n"
        f"Cache: {args.cache}\n"
        f"Findings at severity {min_severity}+: {len(unique)}\n"
        f"Reported tokens: {prompt_tokens} prompt + "
        f"{completion_tokens} completion"
    )

    if errors:
        print(
            f"{len(errors)} request(s) failed; rerun to retry (successful chunks are cached).",
            file=sys.stderr,
        )
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
