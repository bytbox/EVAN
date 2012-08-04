#include "test.hh"

#include "util.hh"
using util::cli_arguments;

#include <cassert>

namespace _cli_arguments {
SUITE("cli_arguments", "util");

test t1("flag", s, ([](){
	assert(cli_arguments({}).flag("a") == false);
	assert(cli_arguments({"-ab"}).flag("ab") == true);
}));

test t2("opt", s, ([](){

}));

test t3("opt-default", s, ([](){

}));

test t4("args", s, ([](){

}));

};

