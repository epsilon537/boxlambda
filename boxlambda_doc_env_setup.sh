#! /bin/bash

#This script does not make any file system modifications outside of the boxlambda directory tree.
echo "Setting up BoxLambda documentation environment. Installing tools if needed."
echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [ "${BASH_SOURCE-}" = "$0" ]; then
    echo "You must source this script: \$ source $0" >&2
    exit 1
fi

if [[ "$#" > 0 && "$1" == "-h" ]]
then
  echo "$0 [-h]"
  echo "-h: Show help."
  exit 1
fi

#Check if venv directory exists
if [ -d venv ]; then
  echo "venv found. Activating..."
  source venv/bin/activate
else
  echo "No venv found. Creating one..."
  python -m venv venv

  echo "Activating venv..."
  source venv/bin/activate

  echo "Installing required Python packages..."
  if python -m pip install -qq -U -r requirements.txt ; then
    echo "OK"
  else
    "Pip install failed. Aborting..."
    return 1
  fi
fi

echo "Installing gems..."
rm -f Gemfile.lock
if bundle install ; then
  echo "OK"
else
  echo "bundle install failed. Please run 'sudo gem update system' and retry."
  echo "Aborting."
  return 1
fi


echo
echo "Setup complete and environment activated."
echo "Enter 'deactivate' to deactivate the environment."
