#include "canvas.hh"

CanvasView::CanvasView() : QGraphicsView(new CanvasScene), defaultTool(), tool(defaultTool) {

}

CanvasView::~CanvasView() {
	delete defaultTool;
}

QSize CanvasView::minimumSizeHint() const {
	return QSize(50, 50);
}

