#include "canvas.hh"

CanvasComment::CanvasComment(const std::string &c) : content(c) {
	setFlag(ItemIsMovable, true);
	setFlag(ItemIsSelectable, true);

	text = new QGraphicsTextItem(c.data());
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

