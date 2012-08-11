#include "test.hh"

#include "foreign.h"

#include <cassert>
#include <unistd.h>

namespace _foreign_lhco {
SUITE("LHCO", "foreign");
test t1("parse", s, ([](){
	Vec_Foreign events = LHCO_Input("foreign/data/ttjj.lhco");
	assert(events.len == 6);
	Vec_Foreign ev0 = LHCO_Parts(events.data[0]);
	assert(ev0.len == 8);
	Vec_Foreign ev1 = LHCO_Parts(events.data[1]);
	assert(ev1.len == 7);
}));
};
