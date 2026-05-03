#! /bin/bash

echo "Note: This script should be sourced from a boxlambda workspace root directory."

git merge jekyll-split --allow-unrelated-histories
git checkout --theirs .
git add .
git commit -m 'Releasing Blog.'
