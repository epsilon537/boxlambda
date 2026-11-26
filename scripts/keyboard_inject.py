#!/usr/bin/env python3
"""
Inject a Linux evdev keyboard into BoxLambda's USB-HID keyboard registers.

The target-side USB-HID driver must already be running and have its
USB_REPORT interrupt enabled. This script only writes KEY_MODS and KEYS.

Example:
    python boxlambda_keyboard_inject.py --device /dev/input/event7 --base 0xXXXXXXXX
    python boxlambda_keyboard_inject.py --list
"""

import argparse
import signal
import socket
import sys
from typing import Dict, Optional, Set

try:
    from evdev import InputDevice, ecodes, list_devices
except ImportError:
    print(
        "Missing dependency: install python-evdev (Arch: sudo pacman -S python-evdev)",
        file=sys.stderr,
    )
    raise SystemExit(2)


# Register offsets from usb_hid_0.yaml.
REG_STATUS = 0x08

REG_KEY_MODS = 0x0C
REG_KEYS = 0x10

# USB HID boot-keyboard modifier byte, matching KEY_MODS bit positions.
MODIFIER_USAGES = {
    ecodes.KEY_LEFTCTRL: 1 << 0,
    ecodes.KEY_LEFTSHIFT: 1 << 1,
    ecodes.KEY_LEFTALT: 1 << 2,
    ecodes.KEY_LEFTMETA: 1 << 3,
    ecodes.KEY_RIGHTCTRL: 1 << 4,
    ecodes.KEY_RIGHTSHIFT: 1 << 5,
    ecodes.KEY_RIGHTALT: 1 << 6,
    ecodes.KEY_RIGHTMETA: 1 << 7,
}


def linux_key_to_hid() -> Dict[int, int]:
    """Build Linux evdev KEY_* -> USB HID keyboard usage mapping."""
    pairs = {}

    def add(linux_name: str, usage: int) -> None:
        code = getattr(ecodes, "KEY_" + linux_name, None)
        if code is not None:
            pairs[code] = usage

    # A-Z: HID usages 0x04..0x1d.
    for i, letter in enumerate("ABCDEFGHIJKLMNOPQRSTUVWXYZ"):
        add(letter, 0x04 + i)

    # 1..9, 0: HID usages 0x1e..0x27.
    for i, digit in enumerate("123456789"):
        add(digit, 0x1E + i)
    add("0", 0x27)

    fixed = {
        "ENTER": 0x28,
        "ESC": 0x29,
        "BACKSPACE": 0x2A,
        "TAB": 0x2B,
        "SPACE": 0x2C,
        "MINUS": 0x2D,
        "EQUAL": 0x2E,
        "LEFTBRACE": 0x2F,
        "RIGHTBRACE": 0x30,
        "BACKSLASH": 0x31,
        "102ND": 0x32,
        "SEMICOLON": 0x33,
        "APOSTROPHE": 0x34,
        "GRAVE": 0x35,
        "COMMA": 0x36,
        "DOT": 0x37,
        "SLASH": 0x38,
        "CAPSLOCK": 0x39,
        "PRINT": 0x46,
        "SCROLLLOCK": 0x47,
        "PAUSE": 0x48,
        "INSERT": 0x49,
        "HOME": 0x4A,
        "PAGEUP": 0x4B,
        "DELETE": 0x4C,
        "END": 0x4D,
        "PAGEDOWN": 0x4E,
        "RIGHT": 0x4F,
        "LEFT": 0x50,
        "DOWN": 0x51,
        "UP": 0x52,
        "NUMLOCK": 0x53,
        "KPSLASH": 0x54,
        "KPASTERISK": 0x55,
        "KPMINUS": 0x56,
        "KPPLUS": 0x57,
        "KPENTER": 0x58,
        "KP1": 0x59,
        "KP2": 0x5A,
        "KP3": 0x5B,
        "KP4": 0x5C,
        "KP5": 0x5D,
        "KP6": 0x5E,
        "KP7": 0x5F,
        "KP8": 0x60,
        "KP9": 0x61,
        "KP0": 0x62,
        "KPDOT": 0x63,
        "102ND": 0x64,
        "COMPOSE": 0x65,
        "POWER": 0x66,
        "KPEQUAL": 0x67,
        "F1": 0x3A,
        "F2": 0x3B,
        "F3": 0x3C,
        "F4": 0x3D,
        "F5": 0x3E,
        "F6": 0x3F,
        "F7": 0x40,
        "F8": 0x41,
        "F9": 0x42,
        "F10": 0x43,
        "F11": 0x44,
        "F12": 0x45,
        "F13": 0x68,
        "F14": 0x69,
        "F15": 0x6A,
        "F16": 0x6B,
        "F17": 0x6C,
        "F18": 0x6D,
        "F19": 0x6E,
        "F20": 0x6F,
        "F21": 0x70,
        "F22": 0x71,
        "F23": 0x72,
        "F24": 0x73,
        "HELP": 0x75,
        "MENU": 0x76,
        "UNDO": 0x7A,
        "CUT": 0x7B,
        "COPY": 0x7C,
        "PASTE": 0x7D,
        "FIND": 0x7E,
        "MUTE": 0x7F,
        "VOLUMEDOWN": 0x81,
        "VOLUMEUP": 0x80,
    }
    for name, usage in fixed.items():
        add(name, usage)

    return pairs


KEY_TO_HID = linux_key_to_hid()


class OpenOCDTcl:
    """Minimal client for OpenOCD's Tcl RPC port (default TCP 6666)."""

    def __init__(self, host: str, port: int, timeout: float = 5.0):
        self.sock = socket.create_connection((host, port), timeout=timeout)
        self.sock.settimeout(timeout)

    def command(self, command: str) -> str:
        self.sock.sendall(command.encode("ascii") + b"\x1a")
        response = bytearray()
        while True:
            chunk = self.sock.recv(4096)
            if not chunk:
                raise ConnectionError("OpenOCD closed the Tcl connection")
            marker = chunk.find(b"\x1a")
            if marker >= 0:
                response.extend(chunk[:marker])
                # Tcl RPC responses are terminated by 0x1a. Commands used here
                # produce no trailing response data.
                return response.decode("utf-8", errors="replace").strip()
            response.extend(chunk)

    def write32(self, address: int, value: int) -> None:
        # mww is OpenOCD's memory-write-word command.
        self.command(f"mww 0x{address:08x} 0x{value & 0xFFFFFFFF:08x}")

    def close(self) -> None:
        self.sock.close()


class KeyboardInjector:
    def __init__(self, ocd: OpenOCDTcl, base: int, verbose: bool = False):
        self.ocd = ocd
        self.base = base
        self.verbose = verbose
        self.pressed: Set[int] = set()
        self.modifiers = 0
        status_addr = self.base + REG_STATUS
        self.ocd.command(f"mww 0x{status_addr:08x} 0x9")

    def send_report(self) -> None:
        # The hardware register has four 8-bit key slots (KEY_0..KEY_3).
        usages = sorted(KEY_TO_HID[key] for key in self.pressed if key in KEY_TO_HID)

        if len(usages) > 4:
            # HID ErrorRollOver usage in every available key slot.
            usages = [0x01] * 4
        else:
            usages += [0] * (4 - len(usages))

        keys_word = sum(usage << (8 * i) for i, usage in enumerate(usages))

        # Keep these ordered: writing KEYS is what triggers the USB report IRQ.
        # Use one Tcl request so the two writes are executed consecutively.
        mods_addr = self.base + REG_KEY_MODS
        keys_addr = self.base + REG_KEYS
        self.ocd.command(
            f"mww 0x{mods_addr:08x} 0x{self.modifiers:02x}; mww 0x{keys_addr:08x} 0x{keys_word:08x}"
        )

        if self.verbose:
            print(f"mods=0x{self.modifiers:02x} keys={usages} (KEYS=0x{keys_word:08x})")

    def handle_key(self, code: int, value: int) -> None:
        # evdev values: 0=release, 1=press, 2=autorepeat.
        if value == 2:
            return

        if code in MODIFIER_USAGES:
            bit = MODIFIER_USAGES[code]
            if value == 1:
                self.modifiers |= bit
            else:
                self.modifiers &= ~bit
            self.send_report()
            return

        if code not in KEY_TO_HID:
            return

        if value == 1:
            self.pressed.add(code)
        else:
            self.pressed.discard(code)

        self.send_report()

    def release_all(self) -> None:
        self.pressed.clear()
        self.modifiers = 0
        self.send_report()


def print_devices() -> None:
    for path in list_devices():
        try:
            dev = InputDevice(path)
            caps = dev.capabilities()
            if ecodes.EV_KEY in caps:
                print(f"{path}: {dev.name}")
            dev.close()
        except OSError:
            pass


def choose_device(path: Optional[str], name: Optional[str]) -> InputDevice:
    if path:
        return InputDevice(path)

    if name:
        matches = []
        for candidate in list_devices():
            try:
                dev = InputDevice(candidate)
                if name.lower() in dev.name.lower():
                    matches.append(dev)
                else:
                    dev.close()
            except OSError:
                continue
        if len(matches) == 1:
            return matches[0]
        for dev in matches:
            print(f"Matching device: {dev.path}: {dev.name}", file=sys.stderr)
            dev.close()
        if not matches:
            raise RuntimeError(f"No input device name contains {name!r}")
        raise RuntimeError("More than one device matched; specify --device")

    raise RuntimeError("Specify --device or --name (or use --list)")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Forward a Linux evdev keyboard to BoxLambda's USB-HID registers."
    )
    parser.add_argument("--device", help="evdev device, e.g. /dev/input/event7")
    parser.add_argument(
        "--name", help="select the unique input device whose name contains this text"
    )
    parser.add_argument("--list", action="store_true", help="list input devices and exit")
    parser.add_argument(
        "--base",
        type=lambda s: int(s, 0),
        default=0x10000040,
        help="USB-HID peripheral base address (default: 0x10000040)",
    )
    parser.add_argument("--host", default="127.0.0.1", help="OpenOCD Tcl host (default: localhost)")
    parser.add_argument("--port", type=int, default=6666, help="OpenOCD Tcl port (default: 6666)")
    parser.add_argument(
        "--grab",
        action="store_true",
        default=False,
        help="grab the Linux keyboard exclusively (default: do not grab)",
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="print injected reports")
    args = parser.parse_args()

    if args.list:
        print_devices()
        return 0
    dev = None
    ocd = None
    grabbed = False
    injector = None

    try:
        dev = choose_device(args.device, args.name)
        print(f"Using {dev.path}: {dev.name}")

        if args.grab:
            dev.grab()
            grabbed = True

        ocd = OpenOCDTcl(args.host, args.port)
        injector = KeyboardInjector(ocd, args.base, args.verbose)

        # Send an initial all-keys-released report to establish a known state.
        injector.send_report()
        if args.grab:
            print(
                "Forwarding keyboard events (device grabbed). Press Ctrl-Alt-Esc or Ctrl-C to stop."
            )
        else:
            print(
                "Forwarding keyboard events (device not grabbed). Press Ctrl-Alt-Esc or Ctrl-C to stop."
            )

        # Track the physical key state separately from the report state. This
        # lets the emergency chord work even when --grab prevents the desktop
        # environment from seeing the keypresses.
        physically_pressed: Set[int] = set()

        for event in dev.read_loop():
            if event.type != ecodes.EV_KEY:
                continue

            if event.value == 1:
                physically_pressed.add(event.code)
            elif event.value == 0:
                physically_pressed.discard(event.code)
            # Ignore autorepeat (value 2) for both state and key forwarding.
            elif event.value == 2:
                continue

            ctrl_down = bool(physically_pressed & {ecodes.KEY_LEFTCTRL, ecodes.KEY_RIGHTCTRL})
            alt_down = bool(physically_pressed & {ecodes.KEY_LEFTALT, ecodes.KEY_RIGHTALT})
            esc_down = ecodes.KEY_ESC in physically_pressed

            if ctrl_down and alt_down and esc_down:
                print("\nEmergency exit: Ctrl-Alt-Esc.")
                break

            injector.handle_key(event.code, event.value)

    except KeyboardInterrupt:
        print("\nStopping.")
    except (OSError, RuntimeError, ConnectionError, socket.timeout) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    finally:
        if injector is not None and ocd is not None:
            try:
                injector.release_all()
            except Exception as exc:
                print(f"Warning: could not send final key-release report: {exc}", file=sys.stderr)
        if grabbed and dev is not None:
            try:
                dev.ungrab()
            except OSError:
                pass
        if dev is not None:
            dev.close()
        if ocd is not None:
            ocd.close()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
