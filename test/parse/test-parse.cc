#include "parse.hh"
#include "program.hh"

#include <iostream>
#include <string>
using namespace std;

#include <cstdio>

int main(int argc, char *argv[]) {
	int ret = 0;
	for (int i = 1; i < argc; i++) {
		char *fn = argv[i];
		try {
			FILE *fin = fopen(fn, "r");
			Program *program = parseProgram(fin);
		} catch (error *e) {
			ret = 1;
			cerr << fn << ": " << e->get_message() << endl;
		}
	}
	//return ret;
	return 0;
}

