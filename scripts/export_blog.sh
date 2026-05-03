#! /bin/bash

set -euo pipefail

echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <version>"
  exit 1
fi

VERSION=$1

DIR="jekyll"

# Check for any changes (tracked or untracked) under DIR
if ! git diff --quiet -- "$DIR" || \
   ! git diff --cached --quiet -- "$DIR" || \
   [ -n "$(git ls-files --others --exclude-standard -- "$DIR")" ]; then

  echo "❌ Uncommitted changes detected in '$DIR':"
  git status -- "$DIR"
  exit 1
fi

jinja2 README.md.j2 -D target=jekyll -D version=$VERSION > jekyll/about.md

git subtree split --prefix=jekyll -b jekyll-split
git switch gh-pages
git merge jekyll-split --allow-unrelated-histories
git checkout --theirs .
git add .
git commit -m 'Releasing Blog.'
git push
git switch -

