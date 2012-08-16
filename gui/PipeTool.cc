#include "app.hh"
#include "canvas.hh"

PipeTool::PipeTool(CanvasBlock *from) : from(from) {
	qtLogger.debug("Starting pipe");
}

PipeTool::~PipeTool() {

}

void PipeTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Attempting to create pipe");
}

