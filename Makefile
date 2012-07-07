PARTS_UTIL = util/Errors
PARTS_PROGRAM = core/Program core/Pipe core/Block core/Each core/Param
PARTS_INTERP = interp/Value interp/Interpreter interp/BlockInterpreter interp/EachInterpreter
PARTS_GUI =

GUI_PARTS = gui/main ${PARTS_PROGRAM} ${PARTS_INTERP} ${PARTS_UTIL}
TEST_PARTS = test/main ${PARTS_PROGRAM} ${PARTS_INTERP} ${PARTS_UTIL}
PARTS = evan ${PARTS_PROGRAM} ${PARTS_INTERP} ${PARTS_UTIL}

HEADERS = include/program.hh include/interp.hh include/util.hh

CXX = c++
CXXFLAGS = -Wall -Wswitch-enum -Wunused -Wuninitialized -std=c++11 -g -O0 -Iinclude
LD = c++

TEST_SOURCES = ${TEST_PARTS:=.cc}
TEST_OBJECTS = ${TEST_PARTS:=.o}

GUI_SOURCES = ${GUI_PARTS:=.cc}
GUI_OBJECTS = ${GUI_PARTS:=.o}

SOURCES = ${PARTS:=.cc}
OBJECTS = ${PARTS:=.o}

all: tags evan evan-gui evan-test

.depend: ${SOURCES} ${HEADERS}
	${CXX} ${CXXFLAGS} -MM ${SOURCES} > $@
-include .depend

evan: ${OBJECTS}
	${LD} -o $@ ${OBJECTS}

evan-gui: ${GUI_OBJECTS}
	${LD} -o $@ ${GUI_OBJECTS}

evan-test: ${TEST_OBJECTS}
	${LD} -o $@ ${TEST_OBJECTS}

test: evan-test
	./evan-test

tags: ${HEADERS}
	ctags ${HEADERS}

clean:
	${RM} evan evan-gui ${OBJECTS} ${GUI_OBJECTS} .depend tags

.PHONY: all clean test

