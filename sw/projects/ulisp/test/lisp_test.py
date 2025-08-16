from __future__ import unicode_literals
import pexpect
import time
import pyte
import copy
import sys
import re
import os

#TX_CHR_DELAY = 0.001

output = None
testname = None
scrn = pyte.Screen(80, 192)
strm = pyte.ByteStream(scrn)

def init():
    print("Board Reset...")
    os.system('openFPGALoader -b arty_a7_100t -r')
    time.sleep(5)

    print("Starting terminal session...")
    ch = pexpect.spawn(
            '/bin/bash -c "stty rows 192 cols 80 && screen /dev/ttyUSB1 115200"')
    ch.logfile = open('pexpect_log.txt','ab')
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

def test(name):
    global scrn
    global strm
    global output
    global testname

    testname = name

    ch = init()

    ch.sendline("")

    try:
        with open(name+".lisp", "r") as file:
            for line in file:
                for c in line:
                    print(c, end="", flush=True)
                    ch.send(c)
                    #time.sleep(TX_CHR_DELAY)
                time.sleep(0.5)

        ch.sendline("'stop")
        ch.expect("stop", timeout=5)

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

    for line in scrn.display:
        if re.match(r"^start", line):
            started = True

        if started and not ended:
            masked_text = re.sub(r'\d{6,}', mask_digits, line)
            output.append(masked_text+'\n')

        if re.match(r"^end", line):
            ended = True

    if not started:
        print("Test did not start.")
        return False

    if not ended:
        print("Test did not end.")
        return False

    print("Output:")

    for line in output:
        print(line, end='')

    return check()

def save():
    global output
    global testname

    with open(testname+".output.txt", "w") as file:
        for line in output:
            file.write(line)

def attach():
    ch = init()

    ch.interact()

    end(ch)

def all():
    test('vera_map_test')
    test('vera_map_test_cont')
    test('vera_map_test_err')
    test('vera_tileset_test')
    test('vera_tileset_test_err')
    test('vera_bitmap_test')
    test('vera_bitmap_test_err')
