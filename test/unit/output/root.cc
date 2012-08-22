#include "test.hh"

#include <cassert>

namespace _root {
suite s("root", module::get("output"));
test t1("all", s, ([](){
}));
};

