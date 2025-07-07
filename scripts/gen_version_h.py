#!/usr/bin/env python

import subprocess

s = subprocess.check_output('git describe --tags --always --dirty',
                            shell=True).decode("utf-8").rstrip("\n")

print("#ifndef VERSION_STR_H")
print("#define VERSION_STR_H")
print(f'#define VERSION_STR "{s}"')
print("#endif /*VERSION_STR_H*/")
