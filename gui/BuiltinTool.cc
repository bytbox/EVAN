#include "builtins.hh"

#include "app.hh"
#include "canvas.hh"

BuiltinTool::BuiltinTool(const Builtin &b) : builtin(b) {

}

BuiltinTool::~BuiltinTool() {

}

void BuiltinTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Placing builtin: " + builtin.name);
	Block *block = new Block(builtin.name, {}, {});
	block->extraInfo.position[0] = p.x();
	block->extraInfo.position[1] = p.y();
	s->add(block);
}

