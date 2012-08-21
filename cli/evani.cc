#include "interp.hh"
#include "parse.hh"
#include "program.hh"
#include "output.h"
#include "util.hh"
using namespace util;

#include <iostream>
#include <string>
using namespace std;

#include <cstdio>

int main(int argc, char *argv[]) {
	cli_arguments args(argc, argv);
	if (args.flag("h") || args.flag("help")) {
		cout << "usage: " << argv[0] << " [options]" << endl;
		cout << "options:" << endl;
		cout << "  -output=METHOD         use the specified output method" << endl;
		cout << "  -output-opts=OPTS      options for the selected output method" << endl;
		cout << "  -output-dest=FILENAME  write output to the specified file" << endl;
		return 0;
	}
	try {
		Program *program = parseProgram(stdin);
		ProgramInterpreter *pi = new ProgramInterpreter(program);
		auto v = pi->next(Interpreter::Scope::empty.into()).get();

		output_method_t meth = output_method_from_name(args.opt("output", "plain").c_str());
		const char *outopts = args.opt("output-opts", "").c_str();
		string outfname = args.opt("output-dest", "");
		output_destination_t dest;
		if (outfname == "") dest = output_to_stdout();
		else dest = output_to_basename(outfname.c_str());

		switch (v.type) {
		case Value::BOOL:
			output_number(meth, dest, NULL, ((bool)v)?1:0);
			break;
		case Value::INT:
		case Value::FLOAT:
			output_number(meth, dest, NULL, v.asDouble());
			break;
		case Value::VEC:
			// TODO handle 2d histograms
			if (args.flag("bars")) {
				vector <Value> vs = v.vec();
				int sz = vs.size();
				double ds[sz];
				for (int i = 0; i < sz; i++)
					ds[i] = vs[i].asDouble();
				output_bars(meth, dest, NULL, sz, ds);
			} else { // histogram
			}
			break;
		case Value::LIST:
			// TODO handle 2d histograms
			if (args.flag("bars")) {
				list <Value> vs = v.lst();
				int sz = vs.size();
				double ds[sz];
				auto it = vs.begin();
				for (int i = 0; i < sz; i++)
					ds[i] = (*it++).asDouble();
				output_bars(meth, dest, NULL, sz, ds);
			} else { // histogram
			}
			break;
		case Value::FOREIGN:
			cerr << "Can't display foreign values" << endl;
			// TODO
			break;
		case Value::BOT:
		default:
			throw (new impossible_error())->with(_POS);
		}
	} catch (string s) {
		cerr << "Caught: " << s << endl;
	} catch (util::error *err) {
		cerr << "Caught error: " << err->get_message() << endl;
		cerr << "  at: " << string(err->position) << endl;
	}
	return 0;
}

