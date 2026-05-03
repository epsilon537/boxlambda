#! /bin/bash

echo "Note: This script should be sourced from a boxlambda workspace root directory."

VERSION=`git branch --show-current`

cp -f Gemfile jekyll/Gemfile
jinja2 README.md.j2 -D target=jekyll -D version=$VERSION > jekyll/about.md

bundle exec jekyll serve --source jekyll --drafts

