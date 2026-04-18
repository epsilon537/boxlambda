# The BoxLambda OS Linker Script

[sw/projects/boxlambda_os/link.ld](../../../sw/projects/boxlambda_os)

The BoxLambda OS linker script is derived from [link_ddr_boot.ld](../../../sw/components/bootstrap/link_ddr_boot.ld),
used by most BoxLambda test builds. It contains additional [sections and variables for the Forth subsystem](../forth/core.md#forth-linker-sections-and-variables) and the RAM disk region `fs_mem`.

See [Linker Script Details](../../build-sys/software/build-struct.md#linker-script-details) for more info.

