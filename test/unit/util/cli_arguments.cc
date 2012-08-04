#include "test.hh"

#include "util.hh"
using util::cli_arguments;

#include <cassert>

namespace _cli_arguments {
SUITE("cli_arguments", "util");

test t1("flag", s, ([](){
	assert(cli_arguments({}).flag("a") == false);
	assert(cli_arguments({"-ab"}).flag("ab") == true);
	assert(cli_arguments({"=ab"}).flag("ab") == false);
	assert(cli_arguments({"ab"}).flag("ab") == false);
}));

test t2("opt", s, ([](){
	assert(cli_arguments({}).opt("a") == "");
	assert(cli_arguments({"-a"}).opt("a") == "");
	assert(cli_arguments({"-a="}).opt("a") == "");
	assert(cli_arguments({"-a=bc"}).opt("a") == "bc");
}));

test t3("opt-default", s, ([](){
	assert(cli_arguments({}).opt("a", "b") == "b");
	assert(cli_arguments({"-a"}).opt("a", "b") == "b");
	assert(cli_arguments({"-a="}).opt("a", "b") == "");
	assert(cli_arguments({"-a=bc"}).opt("a", "b") == "bc");
}));

test t4("args", s, ([](){
	assert(cli_arguments({"-a", "b"}).args().size() == 1);
}));

};

