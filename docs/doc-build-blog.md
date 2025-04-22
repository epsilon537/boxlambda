---
hide:
  - toc
---

# Building the Jekyll Blog

Ruby and Bundler are required if you want to build the BoxLambda Blog website.
```
sudo apt-get install rubygems ruby-bundler
```

Check out the `gh-pages` branch:
```
git checkout gh-pages
```

Source the `boxlambda_setup.sh` script. This will ensure that the gems needed to build Jekyll are installed:
```
source boxlambda_doc_env_setup.sh
```

Make the `jekyll` target:
```
make jekyll
```

Point your browser to [http://127.0.0.1:4000/boxlambda/](http://127.0.0.1:4000/boxlambda/) to see the Jekyll Blog website.

When done, deactivate the documentation environment by executing the `deactivate` command.

