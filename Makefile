PARTS_UTIL = 
PARTS_PROGRAM = Program Pipe Block Each Param
PARTS_INTERP = Value Interpreter BlockInterpreter EachInterpreter
PARTS = evan ${PARTS_PROGRAM} ${PARTS_INTERP} ${PARTS_UTIL}

CXX = c++
CXXFLAGS = -Wall -std=c++11 -g -O0
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
	${RM} evan *.o .depend tags

.PHONY: all clean

