#include "app.hh"
#include "canvas.hh"

PipeTool::PipeTool(CanvasBlock *from) : from(from) {
	qtLogger.debug("Starting pipe");
}

PipeTool::~PipeTool() {

}

void PipeTool::apply(CanvasScene *s, const QPointF &p, std::function<void()> f) const {
	qtLogger.debug("Attempting to create pipe");
	CanvasItem *i = (CanvasItem *)s->itemAt(p);
	if (!i) {
		qtLogger.debug("Pipe creation cancelled");
		return;
	}
	i->createPipe(s, from, i->mapFromScene(p));
}

