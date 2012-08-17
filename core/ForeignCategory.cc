#include "foreigns.hh"

#include <string>
#include <vector>
using namespace std;

ForeignCategory::ForeignCategory(const string &name, const string &cname, const std::vector<ForeignFunc> &fs)
: name(name), cname(cname), foreignFuncs(fs) {}

