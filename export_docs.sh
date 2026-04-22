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
#     export_docs.sh master

echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <banch>"
  exit 1
fi

BRANCH=$1

rm -rf site_docs
python3 scripts/export_docs.py --ref $BRANCH
python3 scripts/gen_full_index.py
cp README.md site_docs/index.md

mkdocs serve

