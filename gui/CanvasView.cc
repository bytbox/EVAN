#include "canvas.hh"

CanvasView::CanvasView() : QGraphicsView(new CanvasScene), defaultTool(), tool(defaultTool) {

}

QSize CanvasView::minimumSizeHint() const {
	return QSize(50, 50);
}

