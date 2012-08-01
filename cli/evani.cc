#include "parse.hh"

#include "program.hh"

#include <iostream>
#include <string>
using namespace std;

#include <cstdio>

int main(int argc, char *argv[]) {
	try {
		Program *program = parseProgram(stdin);
	} catch (string s) {
		cerr << "Caught: " << s << endl;
	}
	return 0;
}

