_project=Stream_Tools
prefix=$(shell dirname $(shell dirname $(shell which gnatls)))
bindir=${prefix}/bin
datadir=${prefix}/share/proj
docdir=${prefix}/share/doc/proj

help:
	echo Help text
	echo ${prefix}
	echo ${bindir}
	echo ${datadir}
	echo ${docdir}

setup:
	# Do set up and code generation

all:

compile:
	gprbuild -p -j0 -P ${_project}

generate-tests:
	gnattest -P ${_project}

compile-test:
	gprbuild -p -P ${_project}-test

generate_tests:
	gnattest -P ${_project}

test:
	bin/${_project}-test-main

dist:


install:
	gprinstall -p -P ${_project}
uninstall:
	gprinstall --uninstall -p -P ${_project}

clean:
	gprclean -P ${_project}
	rm -rf .obj/* bin/*

.PHONY:
