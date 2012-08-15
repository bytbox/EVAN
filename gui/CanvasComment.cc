#include "app.hh"
#include "canvas.hh"

CanvasComment::CanvasComment(const Comment *c) : comment(c) {
	setFlag(ItemIsMovable, true);

	qtLogger.debug("Positioning comment at " + asString<int>(c->extraInfo.position.get(0)));
	qtLogger.debug("Positioning comment at " + asString<int>(c->extraInfo.position.get(1)));
	setPos(c->extraInfo.position.get(0), c->extraInfo.position.get(1));
	text = new QGraphicsTextItem(c->content.data());
	rect = new QGraphicsRectItem();

	updateText();
}

CanvasComment::~CanvasComment() {
	delete rect;
	delete text;
}

void CanvasComment::updateText() {
	rect->setRect(text->boundingRect());
}

QRectF CanvasComment::boundingRect() const {
	return text->boundingRect();
}

void CanvasComment::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	rect->paint(p, sogi, w);
	text->paint(p, sogi, w);
}

