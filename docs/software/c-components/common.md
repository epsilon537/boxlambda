# Common Utilities

- **Commont Software Component in the BoxLambda Directory Tree**:
  [sw/components/common/](../../../sw/components/common)

- **Included in OS**: Yes

The `common` software component contains some common definitions used by other software components and projects.

- [fatal.h](../../../sw/components/common/fatal.h): Defines the `die(<dying gasp message>)` macro.

    Example:

    ```
    if (fr || (fno.fattrib & AM_DIR))
      die("File not found: %s.\n", filename);
    ```

- [inout.h](../../../sw/components/common/inout.h): Defines `IN`, `OUT` and `INOUT` parameter prefixes

    Example: `void foo(IN uint32_t *inp, OUT uint32_t *outp, INOUT uint32_t *inoutp)`

