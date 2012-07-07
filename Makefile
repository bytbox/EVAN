PARTS = evan Program Each Block Link Scope Param Pipe

CXX = c++
LD = c++

HDRS =
SOURCES = ${PARTS:=.cc}
OBJECTS = ${PARTS:=.o}

all: evan

.depend: ${SOURCES} ${HDRS}
	${CXX} ${CXXFLAGS} -MM ${SOURCES} > $@
-include .depend

evan: ${OBJECTS}
	${LD} -o $@ ${OBJECTS}

clean:
	${RM} evan *.o .depend

.PHONY: all clean

