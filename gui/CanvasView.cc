#include "app.hh"
#include "canvas.hh"

CanvasView *CanvasView::view = NULL;

CanvasView::CanvasView()
	: QGraphicsView(new CanvasScene), canvasScene((CanvasScene *)scene()), defaultTool(new DefaultTool), tool(defaultTool) {
	view = this;
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
	QPoint gpos = event->pos();
	QPointF cpos = mapToScene(gpos);
	// get the position from event, but in canvas coordinates
	auto oldTool = tool;
	tool = defaultTool;
	oldTool->apply(canvasScene, cpos, [this, event](){
		qtLogger.debug("CanvasView mouse press");
		super_mousePressEvent(event);
	});
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

