#include "canvas.hh"

CanvasView::CanvasView() : QGraphicsView(new CanvasScene), defaultTool(), tool(defaultTool) {
	setBackgroundRole(QPalette::Base);
	setAutoFillBackground(true);
}

QSize CanvasView::minimumSizeHint() const {
	return QSize(50, 50);
}

void CanvasView::paintEvent(QPaintEvent *ev) {
	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing, true);
	// TODO painter.setClipRegion
}

