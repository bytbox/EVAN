#include "test.hh"

#include "foreign.h"

#include <cassert>
#include <unistd.h>

namespace _foreign_lhco {
SUITE("LHCO", "foreign");
test t1("parse", s, ([](){
	Vec_Foreign events = LHCO_Input("foreign/data/ttjj.lhco");
}));
};
