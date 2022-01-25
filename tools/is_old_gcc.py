#!/usr/bin/env python3
from subprocess import check_output


def versiontuple(v):
    return tuple(map(int, (v.split("."))))


old_version = "4.9.3"
gcc_version = check_output(["gcc", "-dumpversion"]).decode("utf-8")
print(versiontuple(gcc_version) > versiontuple(old_version))
