#include "foreigns.hh"

#include <string>
#include <vector>
using namespace std;

ForeignFunc::ForeignFunc(const string &n, const string &cn, const string &d, const BlockType &t)
: name(n), cname(cn), description(d), type(t)
{}

