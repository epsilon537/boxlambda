#! /bin/bash

# Export docs/ into site_docs/ for Read the Docs / MkDocs.
#
# Features:
# - Copies docs/ tree to site_docs/
# - Rewrites Markdown links that point to source-code files/directories
#   into GitHub blob/tree URLs
# - Preserves normal docs-to-docs relative links
# - Supports branch/tag selection with --ref
#
# Usage:
#     export_docs.sh develop
#     export_docs.sh v0.4.0

echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <version>"
  exit 1
fi

VERSION=$1

rm -rf site_docs
python3 scripts/_export_docs.py --ref $VERSION
python3 scripts/_gen_full_index.py

jinja2 README.md.j2 -D target=github -D version=$VERSION > README.md
jinja2 README.md.j2 -D target=rtd -D version=$VERSION > site_docs/index.md

mkdocs serve

