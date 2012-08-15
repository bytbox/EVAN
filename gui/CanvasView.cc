#include "app.hh"
#include "canvas.hh"

CanvasView::CanvasView()
	: QGraphicsView(new CanvasScene), canvasScene((CanvasScene *)scene()), defaultTool(new DefaultTool), tool(defaultTool) {
	setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
	setResizeAnchor(AnchorUnderMouse);
}

CanvasView::~CanvasView() {
	delete defaultTool;
}

QSize CanvasView::minimumSizeHint() const {
	return QSize(50, 50);
}

void CanvasView::mousePressEvent(QMouseEvent *event) {
	tool->apply(canvasScene, event->pos(), [this, event](){
		qtLogger.debug("CanvasView mouse press");
		super_mousePressEvent(event);
	});
	tool = defaultTool;
}

void CanvasView::super_mousePressEvent(QMouseEvent *event) {
	QGraphicsView::mousePressEvent(event);
}

void CanvasView::setTool(const Tool *t) {
	tool = t;
}

void CanvasView::cancelTool() {
	tool = defaultTool;
}

