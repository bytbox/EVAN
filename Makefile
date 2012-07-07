PARTS_UTIL = util/Errors
PARTS_PROGRAM = core/Program core/Pipe core/Block core/Each core/Param
PARTS_INTERP = interp/Value interp/Interpreter interp/BlockInterpreter interp/EachInterpreter
PARTS = evan ${PARTS_PROGRAM} ${PARTS_INTERP} ${PARTS_UTIL}

CXX = c++
CXXFLAGS = -Wall -Wswitch-enum -Wunused -Wuninitialized -std=c++11 -g -O0 -I.
LD = c++

HDRS =
SOURCES = ${PARTS:=.cc}
OBJECTS = ${PARTS:=.o}
HEADERS = program.hh interp.hh util.hh

all: tags evan

.depend: ${SOURCES} ${HEADERS}
	${CXX} ${CXXFLAGS} -MM ${SOURCES} > $@
-include .depend

evan: ${OBJECTS}
	${LD} -o $@ ${OBJECTS}

tags: ${HEADERS}
	ctags ${HEADERS}

clean:
	${RM} evan ${OBJECTS} .depend tags

.PHONY: all clean

