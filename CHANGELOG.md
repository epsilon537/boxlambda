# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## Label `latency_shakeup`: Changes Since Label `boxlambda_base` - 2025-03-18

### Added

- *boxlambda_setup.sh* includes tools setup, simplifying user setup requirements.
- Added *activate_env.sh* script to activate the tools environment, simplifying user setup.
- Created a *boxlambda_doc_env_setup.sh* script to set up tools environment for building documentation.
- Vivado build aborts with error if timing constraints are not met after implementation step.
- Added a single instruction prefetcher to Ibex. This helps ensure instruction cycle counts are stable and predictable.
- Enabled Vivado post-place and post-route physical optimization.
- Added ibex_perf_test system test. Checking the waveform of this test allows you to check the cycle count of various common instructions: lw and sw to internal memory, lw and sw to SoC registers, addi, branch taken, branch not taken.
- Added CoCoTB module tests for the Ibex single instruction prefetcher, the core2wb adapter, and the wb_staller.

### Fixed

No known issues fixed.

### Changed

- Switched to gdb-multiarch.
- Using system riscv64-unknown-elf toolchain instead of LowRISC GCC toolchain. The LowRISC GCC toolchain does not support multilib which makes it harder to create executables containing strictly uncompressed instructions.
- Updated to OSS CAD Suite 20250226.
- Update to 2025/01/21 version of Ibex code base.
- Update to 2025/01/31 version of Litedram code base.
- Ibex code is imported in Boxlambda repo by gw/components/ibex/gen_core.sh script.
- Switched to Single-Cycle multiplier in Ibex CPU.
- Switched audio DAC test to BoxLambda clock generator.
- Switched to rv32im_zicsr, no instruction compression. Instruction compression may cause unaligned 32-bit instruction fetches, making instruction cycle count undeterministic. Deterministic instruction cycle counts are a BoxLambda requirement.
- Replaced Ibex ICache flag with PrefetchType enum to support PrefetchType_Single option.
- Gave Ibex instruction and data port a shortcut to CMEM resp. DMEM so they don't have to go through the slow crossbar for local memory access.
- Replaced the wbxbar-based shared bus with a faster wb_mux-based shared bus.
- Added transaction separating stallers on the Ibex-to-Crossbar ports to achieve fixed latency for instruction and data fetches going over the crossbar. Without this, transactions on an already open channel complete faster than transactions to a new channel.

### Removed

- User no longer needs to install OSS CAD suite.
- User no longer needs to install Bender.
- User no longer needs to install Python or PIP. This is provided by the OSS CAD Suite environment.
- User no longer needs to install Python packages.
- User no longer needs to install LiteX.
- Removed zeroing out memory and some unnecessary SystemVerilog constructs incompatible with Yosys synthesis, preparing for OpenXC7 support.
- No longer implicitly linkining in the bootstrap component. This now has to be done explicitly by the software project's CMake file.
- No longer implicitly copying the build tree *cmem_to_flash_vector* to source tree's *cmem.mem*. Doing this violates the principle of least astonishment.
- Removed Sounddevice/PortAudio dependency.
- No longer officially supporting WSL. I haven't tried to build BoxLambda on WSL in over a year. The build instruction are most likely obsolete.

## Label `boxlambda_base`: Changes Since Label `dfx` - 2025-01-07

### Added

- Support for multiple reconfigurable partitions in the build system.
- Mechanism to acknowledge accesses to invalid addresses and non-responsive slaves, returning a `0xDEADBEEF` data pattern. This feature is enabled in project builds (`boxlambda_base`, `boxlambda_dfx`) and disabled in test builds.
- Invalid Addressing Test Case.
- Second bus master port added to Reconfigurable Partition `VS0`, enabling `VS0` to support a CPU with Harvard Architecture.
- DFX documentation.
- "Official" BoxLambda Project build `boxlambda_base`, which does not support DFX.
- "Official" BoxLambda Project build `boxlambda_dfx`, which supports DFX.
- Gateware build flags: `VS0`, `DFX`, and `ACK_INVALID_ADDR`.
- Two build variants introduced for all software builds: `<sw_project>_ram` (RAM image) and `<sw_project>_flsh` (Flash Memory image).
- `dfx_load_rm()` helper function in `dfx_controller_hal` for loading a reconfigurable module from memory into Virtual Socket 0 (`VS0`).

### Fixed

- Verilator script fails if VERILATOR_CPP_FLAGS not set in Makefile.
- Build trees not clean after running boxlambda_setup.sh.

### Changed

- Renamed IRQ ID and handler for `VS_1` to `VERA`.
- Renamed directory `gw/projects/boxlambda_top/` to `gw/projects/boxlambda_base/`.
- Renamed CMake function `link_and_create_mem_file` to `link_and_create_image`.
- In gw CMakeLists, replaced VERILATOR_CPP_FLAGS and VERILATOR_LD_FLAGS parameters with a single VERILATOR_FLAGS multi value parameter.
- Instead of relying on a VIVADO_FLAGS environment variable, vivado is now invoked through a wrapper script: *scripts/vivado_wrapper.sh*. The script defines the vivado flags to use.
- Moved all verilator build artifacts except the *Vmodel* executable to a *verilator/* subdirectory of the gw project build directory.

### Removed

- Placeholder IRQ ID and handler for `VS_2`.
- ICAP IRQ handler.
- Deprecated support for the Arty-A7-35T platform.
- Ncurses dependency.

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

[boxlambda_base]: https://github.com/epsilon537/boxlambda/compare/boxlambda_base...dfx
[dfx]: https://github.com/epsilon537/boxlambda/compare/i2c_rtcc...dfx

