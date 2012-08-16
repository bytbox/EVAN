#include "builtins.hh"

#include "app.hh"
#include "canvas.hh"

#include <vector>
using namespace std;

BuiltinTool::BuiltinTool(const Builtin &b) : builtin(b) {

}

BuiltinTool::~BuiltinTool() {

}

void BuiltinTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Placing builtin: " + builtin.name);

	vector<Pipe *> args;
	for (unsigned int i=0; i < builtin.type.args().size(); i++)
		args.push_back(NULL);
	Block *block = new Block(builtin.name, {}, args);
	block->extraInfo.position[0] = p.x();
	block->extraInfo.position[1] = p.y();
	s->add(block);
}

