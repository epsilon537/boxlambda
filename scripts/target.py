#!/usr/bin/env python3

# BoxLambda target interaction script.
# This script is a wrapper around openocd, openFPGAloader and mcopy.
# It allows the user to:
# - reset the target
# - flash or load a bitstream, bootloader, and/or application image.
# - upload or download a ram disk image.
# - attach a debugger.

import argparse
import sys
import subprocess
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
OPENOCD_CFG = f"{SCRIPT_DIR}/openocd.cfg"

def parse_args():
    parser = argparse.ArgumentParser(
        description="Load and control BoxLambda target."
    )

    parser.add_argument(
        "-verilator",
        action="store_true",
        help="Connect to verilator model instead of FPGA."
    )

    parser.add_argument(
        "-flash_bit",
        metavar="FLASH_BITSTREAM",
        type=Path,
        help="flash bitstream file. Ignored if -verilator flag is set.",
    )

    parser.add_argument(
        "-flash_boot",
        metavar="FLASH_BOOT_IMAGE",
        type=Path,
        help="flash bootloader image. Ignored if -verilator flag is set.",
    )

    parser.add_argument(
        "-flash_app",
        metavar="FLASH_APP_IMAGE",
        type=Path,
        help="flash application image. Ignored if -verilator flag is set.",
    )

    parser.add_argument(
        "-load_bit",
        metavar="LOAD_BITSTREAM",
        type=Path,
        help="Load bitstream file. Ignored if -verilator flag is set.",
    )

    parser.add_argument(
        "-reset",
        action="store_true",
        help="Reset the target before performing other actions.",
    )

    parser.add_argument(
        "-load_fs",
        metavar="LOAD_FS_IMAGE",
        type=Path,
        help="Load filesystem image.",
    )

    parser.add_argument(
        "-dump_fs",
        metavar="DUMP_FS",
        type=Path,
        help="Dump filesystem.",
    )

    parser.add_argument(
        "-load_app",
        metavar="LOAD_APP_IMAGE",
        type=Path,
        help="Load application image.",
    )

    parser.add_argument(
        "-run",
        action="store_true",
        help="Execute the application image.",
    )

    parser.add_argument(
        "-gdb",
        action="store_true",
        help="Wait for GDB to connect.",
    )

    return parser.parse_args()


def validate_file(path: Path, label: str):
    if not path.exists():
        print(f"Error: {label} '{path}' does not exist.", file=sys.stderr)
        sys.exit(1)

    if not path.is_file():
        print(f"Error: {label} '{path}' is not a file.", file=sys.stderr)
        sys.exit(1)


def main():
    args = parse_args()

    print("=== Target Control ===")

    # The parsed arguments are mapped to command lists to be executed by
    # the shell, openocd and/or openFPGAloader.

    # List of shell commands to execute before running openocd or openFPGAloader
    shell_cmd_lists_early = []
    openocd_cmd_list = []
    openfpga_cmd_list = []
    # List of shell commands to execute after running openocd or openFPGAloader
    shell_cmd_lists_late = []

    if args.verilator:
        print("Connection to verilator model requested")
        openocd_cmd_list += ["-c", "set VERILATOR 1"]
    else:
        if args.flash_bit:
            validate_file(args.flash_bit, "Bitstream file")
            print(f"Flashing bitstream file: {args.flash_bit}")
            openfpga_cmd_list += ["-f", args.flash_bit]

        if args.flash_boot:
            validate_file(args.flash_boot, "Bootloader image")
            print(f"Flashing bootloader image: {args.flash_boot}")
            openfpga_cmd_list += ["-f", "-o", f"{0x500000}", args.flash_boot]

        if args.flash_app:
            validate_file(args.flash_app, "Application image")
            print(f"Flashing application image: {args.flash_app}")
            openfpga_cmd_list += ["-f", "-o", f"{0x600000}", args.flash_app]

        if args.load_bit:
            validate_file(args.load_bit, "Bitstream file")
            print(f"Loading bitstream file: {args.load_bit}")
            openfpga_cmd_list += [args.load_bit]

    if args.reset:
        print("Reset requested")
        openocd_cmd_list += ["-c", "set RESET 1"]

    if args.load_fs:
        validate_file(args.load_fs, "Filesystem image")
        print(f"Loading filesystem image: {args.load_fs}")
        openocd_cmd_list += ["-c", f"set LOAD_FS {args.load_fs}"]

    if args.dump_fs:
        print(f"Dumping filesystem to dir: {args.dump_fs}")
        shell_cmd_lists_early.append(["rm", "-f", f"{args.dump_fs}.img"])
        shell_cmd_lists_early.append(["rm", "-rf", args.dump_fs])
        openocd_cmd_list += ["-c", f"set DUMP_FS {args.dump_fs}.img"]
        shell_cmd_lists_late.append(["mkdir", args.dump_fs])
        shell_cmd_lists_late.append(["mcopy", "-i", f"{args.dump_fs}.img", "-s", "::*", args.dump_fs])

    if args.load_app:
        validate_file(args.load_app, "Application image")
        print(f"Loading application image: {args.load_app}")
        openocd_cmd_list += ["-c", f"set LOAD_APP {args.load_app}"]

    if args.run:
        print("Run requested")
        openocd_cmd_list += ["-c", "set RUN 1"]

    if args.gdb:
        print("Wait for GDB requested")
        openocd_cmd_list += ["-c", "set GDB 1"]

    # Now we execute the command lists:

    if shell_cmd_lists_early:
        for cmd_list in shell_cmd_lists_early:
            subprocess.run(["echo"] + cmd_list)
            subprocess.run(cmd_list)

    if openfpga_cmd_list:
        openfpga_cmd_list = ["openFPGALoader", "-b", "arty_a7_100t"] + openfpga_cmd_list
        subprocess.run(["echo"] + openfpga_cmd_list)
        subprocess.run(openfpga_cmd_list)

    if openocd_cmd_list:
        openocd_cmd_list = ["openocd"] + openocd_cmd_list + ["-f", OPENOCD_CFG]
        subprocess.run(["echo"] + openocd_cmd_list)
        subprocess.run(openocd_cmd_list)

    if shell_cmd_lists_late:
        for cmd_list in shell_cmd_lists_late:
            subprocess.run(["echo"] + cmd_list)
            subprocess.run(cmd_list)

    if not (openfpga_cmd_list or openocd_cmd_list):
        print("Nothing to do. Use -h for help.")

if __name__ == "__main__":
    main()

