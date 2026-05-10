#! /bin/bash

echo "Note: This script should be sourced from a boxlambda workspace root directory."

VERSION=`git branch --show-current`

cp -f Gemfile blog/Gemfile
jinja2 README.md.j2 -D target=jekyll -D version=$VERSION > blog/about.md

bundle exec jekyll serve --source blog --drafts

