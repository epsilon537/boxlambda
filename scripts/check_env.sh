#! /bin/bash

#This scripts checks if the BoxLambda environment has been activated.

#If the Venv is oss-cad-suite, the environment is active.
if [[ "$VIRTUAL_ENV" == *"oss-cad-suite"* ]]; then
  exit 0
else
  echo "BoxLambda environment not active. Please source activate_env.sh."
  exit -1
fi
