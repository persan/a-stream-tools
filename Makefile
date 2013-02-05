_project=stream_tools
prefix=$(shell dirname $(shell dirname $(shell which gnatls)))

includedir=${prefix}/include/${_project}
bindir=${prefix}/bin
libdir=${prefix}/lib/${_project}
datadir=${prefix}/share/${_project}
docdir=${prefix}/share/doc/${_project}
projectdir=${prefix}/lib/gnat

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
	gprbuild -p -j0 -P ${_project}  -XLIBRARY_TYPE=relocatable
	gprbuild -p -j0 -P ${_project}  -XLIBRARY_TYPE=static
	gprbuild -p -j0 -P version.gpr

generate-tests:
	gnattest -P ${_project}

compile-test:
	gprbuild -p -P ${_project}-test

generate_tests:
	gnattest -P ${_project}

test:
	bin/${_project}-test-main

dist:
	echo git clone . $(_project)-$(shell ./version)
	echo rm -rf $(_project)-$(shell ./version)/.git


install:
	#gprinstall -p -P ${_project}
	mkdir -p ${INSTALL_DIR}${includedir}
	mkdir -p ${INSTALL_DIR}${projectdir}
	mkdir -p ${INSTALL_DIR}${libdir}
	cp -f  src/*.ad? ${INSTALL_DIR}${includedir}
	cp -f ${_project}.gpr.in ${INSTALL_DIR}${projectdir}/${_project}.gpr
	cp -rf lib/* ${INSTALL_DIR}${libdir}


uninstall:
	#gprinstall --uninstall -p -P ${_project}
	rm -rf ${INSTALL_DIR}${includedir}
	rm -rf ${INSTALL_DIR}${projectdir}/${_project}.gpr
	rm -rf ${INSTALL_DIR}${libdir}


clean:
	gprclean -P ${_project}
	rm -rf .obj/* bin/* lib/*

.PHONY:
