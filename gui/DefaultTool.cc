#include "canvas.hh"

DefaultTool::~DefaultTool() {

}

void DefaultTool::apply(CanvasScene *s, const QPoint &p, std::function<void()> f) const {
	f();
}

