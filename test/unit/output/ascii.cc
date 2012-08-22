#include "test.hh"

#include <cassert>

namespace _ascii {
suite s("ascii", module::get("output"));
test t1("all", s, ([](){
}));
};

