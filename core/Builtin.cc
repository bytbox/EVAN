#include "builtins.hh"
#include "typecheck.hh"

#include <string>
using namespace std;

Builtin::Builtin(const string &name, const BlockType &t) : name(name), type(t) {}
Builtin::Builtin(const string &name, const string &desc, const BlockType &t) : name(name), description(desc), type(t) {}

