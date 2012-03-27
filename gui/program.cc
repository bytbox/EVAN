#include "program.hh"

#include <vector>
using namespace std;

Program *Program::sample() {
	Program *p = new Program();
	return p;
}

const char *Program::Name() {
	return filename;
}

