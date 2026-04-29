# This module interacts via the serial port (a screen session) and openFPGAloader
# with an Arty A7 running BoxLambda running ulisp.
# It runs any or all of the ulisp scripts in the current directory and checks
# their output against a reference output file.
# The output is expected to match exactly, except for 'big numbers', which are
# replace with XXXXXX strings.

from __future__ import unicode_literals
import pexpect
import time
import pyte
import copy
import sys
import re
import os
import glob
from pathlib import Path

output = None
testname = None

#ulisp uses terminal emulator control codes so we send the received input through
#a pyte terminal emulator session to get clean textual data.
scrn = pyte.Screen(80, 512)
strm = pyte.ByteStream(scrn)

# After running all(), will hold dictionary of all tests run and their Pass/Fail
# result.
result = {}

def init():
    print("Board Reset...")
    os.system('openFPGALoader -b arty_a7_100t -r')
    time.sleep(20)

    print("Starting terminal session...")
    ch = pexpect.spawn(
            '/bin/bash -c "stty rows 192 cols 80 && screen /dev/ttyUSB1 115200"')

    #All output will accummulate in this log file.
    ch.logfile = open('__pycache__/pexpect_log.txt','ab')
    return ch

def end(ch):
    print("Ending terminal session...")
    ch.sendcontrol('a')
    time.sleep(0.1)
    ch.send('k')
    time.sleep(0.1)
    ch.send('y')
    time.sleep(0.1)
    ch.logfile.close()

def check():
    global output
    global testname

    check_passed = True

    try:
        with open(testname+".output.txt", "r") as file:
            for rline, cline in zip(file, output):
                if rline != cline:
                    check_passed = False
                    print("Mismatch:")
                    print("Received:")
                    print(cline)
                    print("Expected:")
                    print(rline)
    except:
        print("Reference file not found")
        check_passed = False

    if check_passed:
        print("Test passed.")
    else:
        print("Test failed.")

    return check_passed

def mask_digits(match):
    return 'X' * len(match.group(0))

def test(name, line_delay=0.5):
    """Run a single lisp test. Pass in the script name without the .lisp
        extension."""
    global scrn
    global strm
    global output
    global testname

    testname = name

    ch = init()

    ch.sendline("")

    # Send the script line by line with pause between lines
    try:
        with open(name+".lisp", "r") as file:
            for line in file:
                print(line, end="", flush=True)
                match = re.search(r"<wait (\d+)>", line)
                if match:
                    number = int(match.group(1))
                    time.sleep(number)
                else:
                    ch.send(line)
                time.sleep(line_delay)

        ch.sendline("'stop_test")
        ch.expect("stop_test", timeout=5)

    except Exception as e:
        print("Exception during test.")
        print(e)
        end(ch)
        return False

    strm.feed(ch.before)

    end(ch)

    started = False
    ended = False
    output = []

    #Check the output, up to one screen worth of data.
    #Note that this will fail if the test outputs more
    #than one terminal emulator screen of data (check number
    #of rows used for the screen).
    for line in scrn.display:
        #Find start marker
        if re.match(r"^start", line):
            started = True

        if started and not ended:
            #Replace larger numbers with XXXXXX.
            masked_text = re.sub(r'\d{6,}', mask_digits, line).rstrip(" ")
            output.append(masked_text+'\n')

        #Find end marker
        if re.match(r"^end", line):
            ended = True

    if not started:
        print("Test did not start:")

        for line in scrn.display:
            print(line)

        return False

    if not ended:
        print("Test did not end:")
        for line in scrn.display:
            print(line)
        return False

    print("Output:")

    for line in output:
        print(line, end='')

    return check()

def save():
    """Save the output of the previously run test as the reference output for
        this test."""
    global output
    global testname

    with open(testname+".output.txt", "w") as file:
        for line in output:
            file.write(line)

def attach():
    """Attach to the ulisp terminal to manually interact with it."""
    ch = init()

    ch.interact()

    end(ch)

def all(save_all=False):
    """Run all .lisp testcases in the current directory and record the
    result"""
    global result

    files = glob.glob("*.lisp")
    result = {}
    for f in files:
        print(f)
        r = test(Path(f).stem)
        result[f] = r
        if save_all:
            save()

    print("All:")
    print(result)

    print("Failed:")
    print([k for (k, v) in result.items() if not v])

