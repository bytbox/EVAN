MODULES = main
HEADERS = fltk.hh
LIBS = -lfltk

SOURCE = ${MODULES:=.cc}
OBJECTS = ${MODULES:=.o}

all: evan

.depends: ${SOURCE} ${HEADERS}
	${CXX} -MM ${SOURCE} > $@

-include .depends

evan: ${OBJECTS}
	${CXX} -o $@ ${LIBS} ${OBJECTS}

clean:
	${RM} evan ${OBJECTS} .depends

.PHONY: all clean

