import os
import time
import pexpect
import pdb
import subprocess

reference_output_file = "forth_core_test.output.txt"
openocd_p = None
ch = None

def init():
    """Set up the target for the test."""

    global openocd_p
    global ch

    # Don't assume the test build is flashed onto the target.
    # Use openocd+gdb to load the test build onto the target.

    print("Board Reset and OpenOCD start...")

    openocd_p = subprocess.Popen(["openocd_arty_a7_100t.sh", "-r"])
    time.sleep(4)

    print("Pexpect starting serial port channel...")

    ch = pexpect.spawn("socat STDIO,raw,echo=0 /dev/ttyUSB1,b1000000,raw,echo=0")

def terminate():
    """Terminate processes and channels created by init()."""

    global openocd_p
    global ch

    print("Terminating...")

    ch.close(force=True)

    while (ch.isalive()):
        time.sleep(1)

    openocd_p.kill()

    while (openocd_p.poll() is None):
        time.sleep(1)

def check(output):
    """Check the test output (passed in as parameter) against the reference output file. Returns True if successful."""

    check_passed = True

    try:
        with open(reference_output_file, "r") as file:
            for rline, cline in zip(file, output.splitlines(True)):
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

def save(output):
    """Save the output of the previously run test as the reference output for
        this test."""

    with open("forth_core_test.output.txt", "w") as file:
        for line in output:
            file.write(line)

def run(saveref):
    """Run the test. Perform the necessary interactions with the target to complete the test. If saveref input parameter is True, the target's output is saved as reference output for subsequent test runs."""

    global ch

    print("Starting forth_core_test...")

    # Load test build onto target using GDB. Openocd is running in the background.
    os.system('''
pushd . && cd ../../../../build/arty-a7-100/sw/projects/forth_core_test &&
gdb forth_core_test -batch -ex "load" -ex "monitor resume" -ex "detach" -ex "quit" &&
popd
''')

    print("Waiting for data entry prompt...")
    ch.expect("PLEASE TYPE UP TO 80 CHARACTERS", timeout=100)

    print("Output so far...")
    output = ch.before.decode()
    print(output)

    print("Entering some characters...")

    ch.send("Some characters...\n\r")

    print("Waiting for test to complete...")
    ch.expect("TESTING COMPLETE", timeout=100)

    print("Output so far...")
    outputPart2 = ch.before.decode()
    print(outputPart2)
    output += outputPart2

    if saveref:
        print("Saving output as reference...")
        save(output)
        return True
    else:
        print("Checking output against reference...")
        return check(output)

def test(saveref=False):
    init()
    res = run(saveref)
    terminate()

    return res

if __name__ == "__main__":
    res = test()
    if res:
        exit(0)
    else:
        exit(-1)

