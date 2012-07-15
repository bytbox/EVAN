#include "test.hh"

#include "util.hh"

void all() {

}

suite s1("error");
test t1("all", s1, all);

RUNSUITE(s1)

