#include "canvas.hh"

DefaultTool::~DefaultTool() {

}

void DefaultTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	f();
}

