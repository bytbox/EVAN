#include "builtins.hh"

#include <string>
#include <vector>
#include <iostream>
using namespace std;

Category::Category(const string &name, const vector<Builtin> &builtins) : name(name), builtins(builtins) {}

