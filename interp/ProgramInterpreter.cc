#include "interp.hh"
#include "util.hh"

ProgramInterpreter::ProgramInterpreter(Program *program) : program(program) {
	resultInterpreter = Interpreter::get(program->result);
}

maybe<Value> ProgramInterpreter::next(Scope s) {
	return resultInterpreter->next(s);
}

