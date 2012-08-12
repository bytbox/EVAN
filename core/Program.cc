#include "program.hh"

#include <vector>
using namespace std;

Program::Program(Pipe *res) : result(res) {}
Program::Program(Pipe *res, vector<Comment *> cs) : result(res), comments(cs) {}

