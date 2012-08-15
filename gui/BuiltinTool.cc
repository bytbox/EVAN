#include "builtins.hh"

#include "app.hh"
#include "canvas.hh"

BuiltinTool::BuiltinTool(const Builtin &b) : builtin(b) {

}

BuiltinTool::~BuiltinTool() {

}

void BuiltinTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Placing builtin: " + builtin.name);
}

