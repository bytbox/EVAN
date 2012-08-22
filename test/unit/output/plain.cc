#include "test.hh"

#include <cassert>

namespace _plain {
suite s("plain", module::get("output"));
test t1("all", s, ([](){
}));
};

