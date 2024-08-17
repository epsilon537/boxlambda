## CLI

- **Embedded CLI Repo**, BoxLambda fork, *boxlambda* branch:
    [https://github.com/epsilon537/embedded-cli](https://github.com/epsilon537/embedded-cli)

- **Embedded CLI Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/embedded-cli/.

- **Embedded CLI Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/embedded_cli](https://github.com/epsilon537/boxlambda/tree/master/sw/components/embedded_cli)

For bringing up and debugging test applications, it's sometimes convenient to have access to a Command-Line Interface (CLI) from which you can easily issue custom commands such as *peekw* (peek word), *pokew* (poke word), *i2cread*, *settime*, etc. The [Embedded CLI library](https://github.com/epsilon537/embedded-cli) makes it easy to add a CLI to an application.

I'm using the Embedded-CLI library as-is. I just added a small helper module to bind the CLI to BoxLambda's UART and to avoid code duplication across test applications:

[boxlambda/sw/components/embedded_cli/embedded_cli_setup.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/embedded_cli/embedded_cli_setup.h)

For an example of the CLI API usage, check the *rtcc_test* app:

[boxlambda/sw/projects/rtcc_test/rtcc_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_test.cpp)
[boxlambda/sw/projects/rtcc_test/rtcc_cli.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_cli.cpp)

