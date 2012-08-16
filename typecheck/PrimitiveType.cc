#include "typecheck.hh"
#include "util.hh"
using namespace util;

#include <string>
using namespace std;

PrimitiveType::PrimitiveType(PrimitiveType::Type t) : type(t) {

}

PrimitiveType PrimitiveType::fromString(const string &t) {
	if (t == "Bool") return PrimitiveType(BOOL);
	if (t == "Int") return PrimitiveType(INT);
	if (t == "Float") return PrimitiveType(FLOAT);
	throw (new internal_error())->with(_POS);
}

string PrimitiveType::toString() const {
	switch (type) {
	case BOOL:
		return "Bool";
	case INT:
		return "Int";
	case FLOAT:
		return "Float";
	}
	throw (new impossible_error())->with(_POS);
}

