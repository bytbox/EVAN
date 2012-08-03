// Here we implement testing hooks not available during ordinary execution.

#include "test.hh"

#include "interp.hh"

void Interpreter::addFunction(const std::string &name, Interpreter::Function func) {
	testFunctions->add(name, func);
}

