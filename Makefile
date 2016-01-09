_project=stream_tools

-include Makefile.conf

__prefix=$(shell dirname $(shell dirname $(shell which gnatls)))
ifeq (${TARGET},"native")
I_TARGET=
else
ifdef TARGET
B_TARGET=--target=${TARGET} -XTARGET=${TARGET}
I_TARGET=--target=${TARGET} -XTARGET=${TARGET}  --prefix=${__prefix}/${TARGET}
endif
endif

all:
Makefile.conf:Makefile # IGNORE
	echo "prefix=${__prefix}" >${@}
	echo "includedir=${__prefix}/include/${_project}" >>${@}
	echo "bindir=${__prefix}/bin" >>${@}
	echo "libdir=${__prefix}/lib/${_project}" >>${@}
	echo "datadir=${__prefix}/share/${_project}" >>${@}
	echo "docdir=${__prefix}/share/doc/${_project}" >>${@}
	echo "projectdir=${__prefix}/lib/gnat" >>${@}
	echo "export PATH:=${CURDIR}/bin:${PATH}" >>${@}
	echo "export TARGET:=${TARGET}" >>${@}

all:
	${MAKE} compile

help:
	echo Help text
	echo ${PATH}
	echo ${bindir}
	echo ${datadir}
	echo ${docdir}

setup:
	# Do set up and code generation

compile:
	gprbuild -p -j0 -P ${_project} ${B_TARGET} -XLIBRARY_TYPE=relocatable
	gprbuild -p -j0 -P ${_project} ${B_TARGET} -XLIBRARY_TYPE=static
	gprbuild -p -j0 -P version.gpr -XTARGET=native -XLIBRARY_TYPE=static
	./bin/version

generate-tests:
	gnattest -P ${_project}

compile-test:
	gprbuild -p -P ${_project}-test

generate_tests:
	gnattest -P ${_project}

test:
	bin/${_project}-test-main

dist:compile
	git clone . $(_project)-$(shell bin/version)
	rm -rf $(_project)-$(shell bin/version)/.git
	tar -czf $(_project)-$(shell bin/version).tgz $(_project)-$(shell bin/version)
	rm -rf $(_project)-$(shell bin/version)

tag:
	${MAKE} compile
	git tag -f "$(shell bin/version --version)-$(shell bin/version --date)"
	${MAKE} dist

install:
	gprinstall -f  -p -P ${_project} ${I_TARGET}


uninstall:
	#gprinstall --uninstall -p -P ${_project}
	rm -rf ${INSTALL_DIR}${includedir}
	rm -rf ${INSTALL_DIR}${projectdir}/${_project}.gpr
	rm -rf ${INSTALL_DIR}${libdir}


clean:
	rm -rf .obj/* bin/* lib/* Makefile.conf *.tgz _* *~

.PHONY:
