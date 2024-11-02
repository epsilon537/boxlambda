# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Label dfx, changes since label i2c_rtcc - 2024-10-29

### Added

- Swapforth fork as a submodule.
- Build system support for Vivado IP (.xci) dependencies.
- Build system support for DFX projects.
- Virtual socket gateware components vs0_stub/j1b.
- Wb_dfx_controller gateware component.
- Wb_arbiter to arbitrate between DFX controller and Debug Module accessing the xbar.
- DFX example/test project gw/projects/dfx_test + sw/projects/dfx_test.
- J1B example/test project gw/projects/j1b_test + sw/projects/j1b_test.
- J1b_hal SW component providing HAL-level interface to proof-on-concept vs0_j1b core.
- Ymodem and ymodem_cli SW components for transfering files to/from BoxLambda over serial port.
- Mem_fs_cli SW component providing CLI-level access to memory management and file system.
- Dfx_controller_hal SW component providing HAL-level access to DFX Controller.
- Bender target dfx_constraints specifying the file containing the DFX-specific constraints (partition block location and dimensions) of a DFX-enabled project.
- Add -ffunction-sections to CFLAGS to reduce CMEM code footprint.

### Fixed

- Submodule updates not getting git pulled by boxlambda_setup.sh -s.
- CMake build tree generation fails when SDL2 is not installed.
- Dependency of FindSDL2 CMake module is too brittle/distro-dependant.
Dependency of FindSDL2 CMake module is too brittle/distro-dependant.
- i2c_test and rtcc_test don't fit in Arty-A7-35T CMEM.

### Changed

- Boxlambda_setup.sh creates build trees fresh (cmake --fresh).
- Gw/CMakeLists.txt replaces vivado flags -nolog -nojournal -notrace with VIVADO_FLAGS environment variable.
- Bender target constraints is now called prj_constraints to differentiate it from dfx_constraints.
- Split gw/ CMake functions creating build rules into _vivado and _verilator variants.
- Rename RM_0/1/2 interrupts to VS_0/1/3 interrupts.
- PicoRV DMA bus master address vector reduced from 30 bits to 28 bits.
- Bender memory target split into memory_verilator and memory_vivado variants.
- Wb_dp_ram_wrapper INIT_FILE is also used in Vivado builds, used to be only Verilator.
- All non-dfx build bitstream files are called project.bin/bit/sw/bit and are written in project directory itself, not project.runs/impl.
- The default contents of CMEM, i.e. after building _bit target, is cmem_to_flash_vector (was: empty). The _bit_sw target overrules this default with the selected SW project.
- Increase Embedded CLI max. bindings to 24.

### Removed

- *Project*_lint target from arty_\* build trees.
- Verilator flags from CMake gw_component_rules().
- Bender.lock file generation.

[dfx]: https://github.com/epsilon537/boxlambda/compare/i2c_rtcc...dfx

