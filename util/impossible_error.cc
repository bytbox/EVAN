#include "util.hh"

#include <string>
using namespace std;

using util::impossible_error;

impossible_error::impossible_error() : internal_error("the impossible happened") {}

