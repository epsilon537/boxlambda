# Building the Jekyll Blog

Ruby and Bundler are required to build the BoxLambda blog website. Install them using the following commands:

```
sudo apt-get install rubygems ruby-bundler
```

Build the blog by executing the following command:

```
make_blog.sh
```

Point your browser to [http://127.0.0.1:4000/boxlambda/](http://127.0.0.1:4000/boxlambda/) to see the Jekyll Blog website.

## Blog Release Protocol (Note to Self)

0. [Release code and documentation](git-workflow.md#creating-a-new-release-note-to-self).
1. Write draft in `develop` branch, `jekyll/_drafts` directory. Use labeled links to code and documentation.
2. Test site using `make_blog`.
3. Check spelling and grammar.
4. Move draft to `jekyll/_posts` and add date to filename.
5. Check for broken links using `lychee -i *.md` in the `jekyll/_posts` directory.
6. Ensure all changes in current branch are committed.
7. Release by executing `export_blog.sh vX.Y.Z`.

