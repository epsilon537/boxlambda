# The Target filesystem

The `fs/` directory in the Boxlambda repo is the root of the target filesystem. Its structure will evolve over time. Currently, it contains two directories:

- `forth/` contains the system's `*.fs` Forth modules.
- `test/` contains test files used by the Forth test suite.

Refer to the [WordList Shell section](forth/words.md#shell) to see how to navigate the target file system from the Forth REPL.

Refer to the [Installation section](../../installation/installation.md) to see how to put the target file system on an SD card or on a target RAM disk.
