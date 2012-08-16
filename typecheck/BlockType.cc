#include "typecheck.hh"

#include <vector>
using namespace std;

BlockType::BlockType(vector<Type *> ps, vector<Type *> as, vector<Type *> rs) 
: parameters(ps), arguments(as), results(rs)
{

}

const std::vector <Type *> BlockType::args() const {
	return arguments;
}

