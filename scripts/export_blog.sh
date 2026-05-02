#! /bin/bash

echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <version>"
  exit 1
fi

VERSION=$1

jinja2 README.md.j2 -D target=jekyll -D version=$VERSION > jekyll/about.md

git subtree push --prefix=jekyll origin gh-pages

