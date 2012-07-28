#include "canvas.hh"

Canvas::Canvas() : defaultTool(), tool(defaultTool) {
	setBackgroundRole(QPalette::Base);
	setAutoFillBackground(true);
}

QSize Canvas::minimumSizeHint() const {
	return QSize(50, 50);
}

void Canvas::paintEvent(QPaintEvent *ev) {
	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing, true);
	// TODO painter.setClipRegion
}

