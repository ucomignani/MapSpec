CAMLC = corebuild

LIBS = bolt,core_bench
LIBS_TEST = oUnit

SOURCES = src/,src/tests/

EXEC = MapSpec.native
EXEC_TEST = test.native

default: main execDir

dataEx: main execDir

quickTest: runTest

test: main execDir testCompile execTestsDir runTest


main:
	corebuild -pkg $(LIBS) -Is $(SOURCES) $(EXEC)

testCompile:
	corebuild -pkg $(LIBS),$(LIBS_TEST) -Is $(SOURCES) $(EXEC_TEST)

runTest:
	BOLT_CONFIG=bolt.config ./exec/tests/test.native

execDir:
	mv *.native exec/

execTestsDir:
	mv *.native exec/tests/

clean:
	corebuild -clean

.PHONY: compile testCompile run clean test default
