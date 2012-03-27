MODULES = main program canvas json
HEADERS = fltk.hh program.hh canvas.hh json.hh

LIBS = -lfltk
LDFLAGS =
CXXFLAGS = -O2 -Wall

SOURCE = ${MODULES:=.cc}
OBJECTS = ${MODULES:=.o}

all: evan

.depends: ${SOURCE} ${HEADERS}
	${CXX} -MM ${SOURCE} > $@

-include .depends

evan: ${OBJECTS}
	${CXX} -o $@ ${LDFLAGS} ${OBJECTS} ${LIBS}

clean:
	${RM} evan ${OBJECTS} .depends

.PHONY: all clean

