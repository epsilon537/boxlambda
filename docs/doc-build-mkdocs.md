# Building the MkDocs Documentation

Python and Pip are required if you want to build the BoxLambda MkDocs documentation.

Check out the `gh-pages` branch:
```
git checkout gh-pages
```

Source the `boxlambda_setup.sh` script. This will create (if needed) and activate a Python venv that contains the required MkDocs packages:
```
source boxlambda_doc_env_setup.sh
```

Make the `mkdocs` target:
```
make mkdocs
```

Point your browser to [http://127.0.0.1:8000/boxlambda/](http://127.0.0.1:8000/boxlambda/) to see the MkDocs website.

When done, deactivate the documentation environment by executing the `deactivate` command.


