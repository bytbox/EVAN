#include "canvas.hh"

CanvasBlock::CanvasBlock(Block *b) : block(b) {
	setFlag(ItemIsMovable, true);

	setPos(b->extraInfo.position.get(0), b->extraInfo.position.get(1));
	text = new QGraphicsTextItem("hi");
	rect = new QGraphicsRectItem();

	rect->setRect(text->boundingRect());
}

CanvasBlock::~CanvasBlock() {
	delete rect;
	delete text;
}

QRectF CanvasBlock::boundingRect() const {
	return text->boundingRect();
}

void CanvasBlock::paint(QPainter *p, const QStyleOptionGraphicsItem *sogi, QWidget *w) {
	rect->paint(p, sogi, w);
	text->paint(p, sogi, w);
}

