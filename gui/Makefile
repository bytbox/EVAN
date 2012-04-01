MODULES = main program canvas json parse lex
HEADERS = fltk.hh program.hh canvas.hh json.hh

LIBS = -lfltk
LDFLAGS =
CXXFLAGS = -O2 -Wall

YFLAGS = -d

SOURCE = ${MODULES:=.cc}
OBJECTS = ${MODULES:=.o}

all: evan

.depends: ${SOURCE} ${HEADERS}
	${CXX} -MM ${SOURCE} > $@

-include .depends

evan: ${OBJECTS}
	${CXX} -o $@ ${LDFLAGS} ${OBJECTS} ${LIBS}

lex.cc: lex.l 
	${LEX} ${LFLAGS} -o $@ $<

parse.cc: parse.y
	${YACC} ${YFLAGS}  -o $@ $<

clean:
	${RM} evan ${OBJECTS} parse.cc parse.hh lex.cc .depends

.PHONY: all clean

