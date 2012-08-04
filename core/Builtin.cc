#include "builtins.hh"

#include <string>
using namespace std;

Builtin::Builtin(const string &name) : name(name) {}
Builtin::Builtin(const string &name, const string &desc) : name(name), description(desc) {}

