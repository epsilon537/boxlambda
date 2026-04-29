# Building the Jekyll Blog

Ruby and Bundler are required to build the BoxLambda blog website. Install them using the following commands:

```
sudo apt-get install rubygems ruby-bundler
```

Check out the `gh-pages` branch:
```
git checkout gh-pages
```

Source the `boxlambda_setup.sh` script to ensure that the gems needed to build Jekyll are installed:

```
source boxlambda_doc_env_setup.sh
```

Build the blog by executing the following command:

```
make jekyll
```

Point your browser to [http://127.0.0.1:4000/boxlambda/](http://127.0.0.1:4000/boxlambda/) to see the Jekyll Blog website.

When you're done, deactivate the documentation environment by running:

```bash
deactivate
```

