#!/usr/bin/make gps -C
_project=stream_tools

-include Makefile.conf

PREFIX?=$(shell dirname $(shell dirname $(shell which gnatls)))


all:compile

Makefile.conf:Makefile
	echo "GPRINSTALL=$(shell which gprinstall)" >${@}


help:
	echo Help text

setup: # IGNORE
	# Do set up and code generation

compile:
	gprbuild -p -j0 -P ${_project} -XLIBRARY_TYPE=relocatable
	gprbuild -p -j0 -P ${_project} -XLIBRARY_TYPE=static
	gprbuild -p -j0 -P ${_project}-version.gpr -XLIBRARY_TYPE=static


.PHONY: test
test:
	${MAKE} -C tests

dist:compile
	git clone . $(_project)-$(shell bin/version)
	rm -rf $(_project)-$(shell bin/version)/.git
	tar -czf $(_project)-$(shell bin/version).tgz $(_project)-$(shell bin/version)
	rm -rf $(_project)-$(shell bin/version)

tag-check: # IGNORE
	if [ ! -z  ` git status --porcelain` ] ; then \
		echo "Folder is not clean" ;\
		git status;\
		exit 1;\
        fi
	${MAKE} compile
	./bin/version --tagcheck

tag:tag-check
	git tag -f "$(shell bin/version --version)-$(shell bin/version --date)"
	${MAKE} dist
	git push --tag

install:uninstall
	${GPRINSTALL} --prefix=${PREFIX}  -p -P ${_project}


uninstall: # IGNORE
	-@if [ -n "$(shell gprinstall --prefix=${PREFIX} --list | grep ${_project})" ]; then \
		${GPRINSTALL} --prefix=${PREFIX} --uninstall -P ${_project} 2>/dev/null 1>&2 ;\
	fi


clean:
	git clean -xdf

.PHONY:
