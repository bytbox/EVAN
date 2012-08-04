#include "parse.hh"

#include <string>
using namespace std;

ParseError::ParseError() : user_error("unknown parse error") {}

ParseError::ParseError(const string &m) : user_error(m) {}

