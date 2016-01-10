#!/usr/bin/env python
import subprocess
print subprocess.check_output(["gcc", "-dumpversion"]) < "4.9.3"
