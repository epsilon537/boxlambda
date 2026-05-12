#! /bin/bash

set -euo pipefail

echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <version>"
  exit 1
fi

VERSION=$1

DIR="blog"

# Check for any changes (tracked or untracked) under DIR
if ! git diff --quiet -- "$DIR" || \
   ! git diff --cached --quiet -- "$DIR" || \
   [ -n "$(git ls-files --others --exclude-standard -- "$DIR")" ]; then

  echo "❌ Uncommitted changes detected in '$DIR':"
  git status -- "$DIR"
  exit 1
fi

jinja2 README.md.j2 -D target=jekyll -D version=$VERSION > blog/about.md
git commit -m "blog/about.md update" blog/about.md

git subtree split --prefix=blog -b blog-split
git switch gh-pages
git merge blog-split --allow-unrelated-histories
git checkout --theirs .
git add .
git commit -m 'Releasing Blog.'
git push
git switch -
git branch -d blog-split
